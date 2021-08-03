==============================================
PEL -- Pragmatic Environment Library for Emacs
==============================================

:URL: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst
:Project:  `PEL Project home page`_
:Modified: 2021-08-03 13:15:54, updated by Pierre Rouleau.
:License:
    Copyright (c) 2020, 2021 Pierre Rouleau <prouleau001@gmail.com>


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

- an Emacs key binding documentation project with **over 130**
  `PDF topic-oriented reference sheets`_ with hyperlinks to Emacs manuals, external
  packages, articles and other useful references.

  - you can either you can access these PDF files directly from Emacs or via
    the `PEL Index PDF`_ that lists all the PEL PDF files and provides a good
    starting point.  For the best user experience use a good web browser like
    Firefox [#firefox]_ version 78 or later that can render the PDF files.

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
    Keys Bindings`_.  It also uses the **F9** key with some external packages.
    - are implemented by a `set of small Emacs Lisp files`_ and that deal
    with several aspects of Emacs like windows and frame, scrolling control,
    buffer, navigation, opening files or web pages from file name or URL at
    point, numeric keypad handling, etc...

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

.. [#firefox] In 2021 Firefox is a highly rated web browser.  See the
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

I also created a set of tables
that each list and describe a specific topic, the commands and key bindings
related to that topic.
There are several topics; Emacs navigation, Emacs
buffers, windows and frames, how to undo, redo, work with Emacs Lisp, etc...
See the `Key Bindings Documentation`_ section.
The commands and key bindings described in those table include what is provided
by the plain vanilla GNU Emacs, the third party packages PEL integrates and the
PEL commands.


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
   This is the first release of PEL, and my first contribution to Emacs,
   written as I learned Emacs.
   It will grow with time, incorporating more documentation,
   support for more Emacs packages related to editing and
   programming tasks.  Don't hesitate to report problems and
   recommend better ways!


Using Portions of PEL Manually
------------------------------

If you prefer not using PEL's key bindings you can `override them`_.
You can also just use the `PEL features`_ you want and create your own key
bindings. In that case, don't call ``pel-init``, require the respective PEL
source code file and create your own key bindings.
The PEL files are listed in each of the corresponding
`PEL Convenience Features`_ section.

.. _override them: `To override or change PEL key bindings`_
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
   one of the the latest stable releases: Emacs 26.3, or 27.2.
#. `Install other required command line tools`_


**Fast Track**

If you don't want to read the full detailed description of the installation
steps, you can skip to the section titled `Fast Track Installation Steps`_.

**Detailed Track**

The fully detailed instructions are described in the following sections:

#. `Clone the PEL Git repository`_ on your computer.
#. `Prepare Emacs Initialization directory`_ where lots of Emacs files will go.
#. `Create a "~/.emacs.d/utils" directory`_ for files from the EmacsAttics_ and
   EmacsMirror_ that PEL uses and for files hosted on GitHub and Gitlab that are not
   supported by Elpa packaging mechanism.
#. `Create the emacs-customization.el file`_.
#. `Create or Update your Emacs init.el file`_.
#. `Byte Compile PEL Files`_.
#. Optional: `Add Support for Fast Startup`_.
#. Optional: `Add Support for Independent Customization of Graphics and
   Terminal based Emacs`_.
#. Optional: `Add Support for Package Quickstart for Emacs 27 and later`_
#. `Activate PEL Features - Customize PEL`_.


Detailed instructions for the above steps are written in the following sections.

If you wish you can also customize PEL and Emacs further.  See the following
sections:

- `Further PEL Customization`_
- `Emacs and PEL Optimizations`_
- `Generic Tips`_, specially `Launching graphics mode Emacs from a shell`_.

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
PEL's files inside it, including all `PDF document tables`_
that document the key bindings of Emacs and the libraries you can activate with
PEL.

.. _PEL's Github repo: https://github.com/pierre-rouleau/pel

**Do this:**

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
- Emacs init file,
- Emacs customization file,
- Emacs bookmarks file,
- Emacs abbreviation files,
- External Emacs Lisp libraries downloaded from Elpa-compliant sites like
  ELPA_, MELPA_ or MELPA-STABLE_,
- External Emacs Lisp libraries from the EmacsAttics_ or EmacsMirror_ that are
  not Elpa-protocol compliant and must be stored into a *utils* directory,
- etc...


It is best to  create the "``~/.emacs.d``" directory and store
Emacs configuration file inside "``~/.emacs.d/init.el``".

The following instructions assume that your Emacs initialization file is
"``~/.emacs.d/init.el``".


Windows users:
  Under Windows, your ".emacs.d" directory should be stored inside your HOME
  directory. See `Emacs Windows init location FAQ`_ for more information.

.. _Emacs Windows init location FAQ: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Location-of-init-file.html

Create a "~/.emacs.d/utils" directory
-------------------------------------

**Description:**

The name of the directory could be anything, *utils* is what this example uses.
Create the "~/.emacs.d/utils"  directory.  This is where you need to store the
single file external packages that PEL uses and which are not supported by the
Elpa-compliant sites.

An easy way to do this from a shell is shown below.

**Do this:**

.. code:: shell

          mkdir -p ~/.emacs.d/utils


Create the emacs-customization.el file
--------------------------------------

**Description:**

Emacs stores customization information inside your ``init.el`` file by default.
It is best to store it somewhere else, as written in section 4 of the
section `Create or Update your Emacs init.el file`_.

By storing it inside "``~/.emacs.d/emacs-customization.el``" you can control
your Emacs customization independently from your Emacs initialization and you
can also copy and distribute the customization file across several computers to
use the same tools the same way.  Since PEL controls activation and download of
the external Emacs Lisp libraries by the PEL user options (with ``pel-use-``
names), the customization will also control the external libraries installed.

Before the next step you must therefore create an empty
"``~/.emacs.d/emacs-customization.el``" file.
This can be done from a terminal shell, as described below.

**Do this:**

.. code:: shell

         touch ~/.emacs.d/emacs-customization.el

**You might also have to do this:**

If you already had a ``custom-set-variables`` form inside your init.el file,
move it inside the "``~/.emacs.d/emacs-customization.el``" file, otherwise the
file can stay empty.  It will be filled by Emacs in the next step.


Create or Update your Emacs init.el file
----------------------------------------

**Do this:**

Add the following code inside your "``~/.emacs.d/init.el``" file.
You can also use a copy of the file `example/init/init-1.el`_ :


.. _example/init/init-1.el: ../example/init/init-1.el

.. code:: elisp

        ;; -*-no-byte-compile: t; -*-
        ;;; ---Example init.el file ---------------- Step 1----------------------------
        ;;
        (defconst pel-home-dirpath (expand-file-name "~/projects/pel")
          "Directory where PEL source files are stored.")

        ;; 1: Setup additional package sources: MELPA, MELPA-STABLE.
        ;;    By default Emacs only identifies the gnu archive located at
        ;;    URL "https://elpa.gnu.org/packages/".
        ;;    Add the MELPA archives as they provide more packages.
        (when (>= emacs-major-version 24)
          (if (< emacs-major-version 27)
              ;; Emacs prior to 27
              ;; ----------------
              (progn
                (require 'package)
                (setq package-enable-at-startup nil)
                (if (member emacs-version '("26.1" "26.2"))
                    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
                (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                                    (not (gnutls-available-p))))
                       (proto (if no-ssl "http" "https")))
                  (add-to-list 'package-archives
                               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
                  (add-to-list 'package-archives
                               (cons "melpa-stable"
                                     (concat proto "://stable.melpa.org/packages/")) t))
                (package-initialize))

            ;; Emacs 27 or later.
            ;; ------------------
            ;; Emacs >= 27 support the `package-quickstart' feature which
            ;; speeds-ups Emacs startup time.  This is a user-option which must be
            ;; activated manually.
            ;; When package-quickstart is customized to t, Emacs 27 support 2 initialization
            ;; files in the user-emacs-directory (which often is ~/.emacs.d), these are:
            ;;
            ;; - early-init.el  : loaded very early in the startup process before
            ;;                    graphical elements are initialized and before the
            ;;                    package manager is initialized.  The following
            ;;                    variables should be set in early-init.el:
            ;;                    - `package-load-list'
            ;;                    - `package-user-dir'
            (unless (boundp 'package-quickstart)
              (setq package-quickstart nil))
            (unless package-quickstart
              ;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
              (let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                                user-emacs-directory)))
                (when (file-exists-p fast-startup-setup-fname)
                  (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
                  (pel-fast-startup-set-builtins)
                  ;; Remember Emacs is running in PEL's fast startup mode.
                  (setq pel-running-with-bundled-packages t))))
            (require 'package)
            (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
            (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
            (package-initialize)))


        ;; 2: Delay loading of abbreviation definitions
        ;;     Disable loading the abbreviation file during Emacs initialization.
        ;;     To do this: save and replace the content of the variable that holds
        ;;     the file name of the abbreviation list with the name of a file
        ;;     that does not exists.
        ;;     Pass the original name to pel-init later to initialize properly.
        ;;
        ;; (setq pel--abbrev-file-name abbrev-file-name)
        ;; (setq abbrev-file-name "~/abbrev_defs-invalid") ; use non-existing file name

        ;; 3: Add pel to Emacs load-path
        ;;    Identify the directory where you stored pel.
        (add-to-list 'load-path pel-home-dirpath)

        ;; 4: Add utils to Emacs load-path
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/utils"))

        ;; 5: Store Emacs customization inside a separate file
        ;;    If you already have a (custom-set-variables ...) form
        ;;    in your init.el, move it into this new file.
        (setq custom-file "~/.emacs.d/emacs-customization.el")
        (load custom-file)

        ;; 6: Start PEL
        ;; - At first leave this commented out.
        ;; - Activate the code Once you have successfully built PEL once
        (require 'pel)
        (pel-init)  ; or later->; (pel-init pel--abbrev-file-name)

        ;;; ---- end of init.el -------------------------------------------------------


**Description:**

- Section 1 of the code adds the following URLs of Elpa-compliant Emacs package
  archives:

  - MELPA_
  - MELPA-STABLE_

- Section 2 delays the loading of the abbreviation lists to after PEL is
  loaded.  This mechanism is described in the section titled
  `Delay Loading of Abbreviation Definition File`_.
- Section 3 adds the location of the *pel* directory to Emacs ``load-path``
  to allow Emacs to find all PEL Emacs Lisp files.  This should be the
  directory where you downloaded PEL.
- Section 4 adds the location of the *utils* directory to Emacs ``load-path`` to
  allow Emacs to find the single file Emacs libraries PEL uses.
- Section 5 tells Emacs to store its customization form inside a file called
  "``~/.emacs.d/emacs-customization.el``".  If you already have Emacs customization
  inside your current init.el file, copy it inside that new file.
  Emacs customization is the full content of the ``(custom-set-variables ...)`` form.
- Section 6 load and initializes PEL by evaluating the ``pel-init`` function that
  is located inside the file ``pel.el``.

.. _cloned PEL: `Clone the PEL Git repository`_

If you have cloned PEL inside ``~/projects/pel`` you are ready to go.
Otherwise you **must update** the init.el to identify the location of the
``pel`` directory as shown below.

.. image:: res/update-init.png

**Note**

If you work inside several projects and each project requires different
Emacs settings, you could use several customization files and activate them
for each project, reducing the load time further.
That provides another degree of freedom, along with Emacs directory local
and file local variables.



Byte Compile PEL Files
----------------------

**Description:**

Use the provide Makefile script to byte-compile all required PEL Emacs Lisp
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

**At this point you can use Emacs with PEL**

You should have a working version of PEL with all
files byte-compiled for efficiency 😅!

If you start Emacs now, PEL will start and will download and install the
following Emacs packages if you don't already have them:

- which-key_ (because ``pel-use-which-key`` is turned on by default)
  and any package(s) which-key_ may require.

The *only* thing left is to use Emacs customization system to activate the
features you want. That's described in the next section.

**In case of Errors:**

If the make script stopped at an error, just repeat the operation listed above.

If the problem persists, or if you see an error or a warning during the build
or when you start Emacs, please `create an issue`_ describing the problem and
your environment and I will get to it.


.. _create an issue: https://github.com/pierre-rouleau/pel/issues

Skip the next section and read the section describing how to configure PEL:
`Activate PEL Features - Customize PEL`_.

.. ---------------------------------------------------------------------------

Optional Steps
--------------

The following steps described in this section are optional:

- With `Add Support for Fast Startup`_ you add more logic in your init.el file
  to support a PEL-specific mode where Emacs starts faster than normally.  PEL
  supports two startup modes: the *normal* startup mode and the *fast* startup
  mode. When operating with the fast startup mode PEL does not support
  automatic download and installation of external packages but it will start
  faster than in the normal mode.  You can switch from one more to the other
  with dedicated commands.
- If you plan to use Emacs in both terminal (TTY) mode and in graphics mode
  you may want to use different options in each.  PEL supports that.  You will
  want to `Add Support for Independent Customization of Graphics and Terminal
  based Emacs`_.
- If you use Emacs 27 or later and want to take advantage of the Emacs
  startup speedup provided by Emacs package quickstart mechanism, then
  you will want to `Add Support for Package Quickstart for Emacs 27 and
  Later`_.



Add Support for Fast Startup
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Description:**

To provide a much faster startup speed when using a large number of external
packages PEL provides logic to bundle all single directory packages inside a
single package directory.  This reduces the length of Emacs load-path list and
improves the startup speed.  PEL provides commands to display the currently
active startup mode and switch from one mode to the other.  See the `Fast
Startup`_ PDF for more information.

To take advantage of this mechanism you must also add logic inside your
init.el file.  PEL provides an example of this code: the
`example/init/init-5.el`_ file.  The file is fully commented and identifies a
set of options you will need to edit.

**Do this:**

- Start an Emacs process that has been configured without fast startup.
- Open a terminal shell.  Inside that shell type the following commands to
  use `example/init/init-5.el`_ as your init.el file:

.. code:: shell

          cd ~/.emacs.d
          mkdir tmp
          cp init.el tmp
          cp ~/projects/pel/example/init/init-5.el init.el


- With the Emacs process you have already identified, use it to edit
  your new ``~/.emacs.d/init.el`` file.

  - To open the file type ``C-x f ~/.emacs.d/init.el``
  - Read the instructions located inside the top of the file.
  - Use ``C-s OPTION RET`` to search for the word "OPTION" and modify the code
    according to what you need:

    - **OPTION A**: if you want to use two independent custom files for terminal
      (TTY) and graphics mode, set ``pel-use-graphic-specific-custom-file-p``
      symbol to **t**.  See the next section,
      `Add Support for Independent Customization of Graphics and Terminal
      based Emacs`_ for more information.
    - **OPTION B**: this is to measure time spent by code executed at startup.
      Unless you know Emacs at this point, leave the code commented out.
      Later, when you have a better understanding of Emacs and ecosystem you
      can come back and activate it.
    - **OPTION C**: By default Emacs displays generic information about GNU
      and Emacs on startup.  After reading it once or twice you may want to
      prevent this information from showing up.  For that un-comment the line
      shown below the OPTION C text and replace the string YOUR_USER_NAME by
      your OS user name.  On Unix-like OS, this is what the **who** command
      displays.
    - **OPTION D**: this is a small section of code that activates or
      de-activates various global Emacs settings.  It starts with a commented
      line that disables the tool bar of Emacs running in graphics mode.  If
      you do not want to use that tool-bar un-comment the corresponding line
      of code.  Read the code in that section.  You may want to modify some of
      this.  However remember that PEL controls Emacs behaviour through
      customization, not by code invoked through the init.el file: it's best
      to minimize what you add the this section of code if you want to take
      advantage of what PEL offers and to minimize Emacs startup time.

  - Save your modifications back to the init.el file by typing ``C-x C-s``
  - Keep Emacs opened on your init.el file.

- Open a new terminal shell.

  - Open Emacs in that new shell.  If all is OK, Emacs should start fine and
    should not display any error message.  If it does display an error message
    then something is probably wrong in your init.el file.  Modify it, save
    the file and try again.

- Once Emacs starts properly close all Emacs sessions.
  You can type ``C-x C-c`` to save all buffers and terminate Emacs.



Add Support for Independent Customization of Graphics and Terminal based Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Emacs can run in terminal (TTY) mode inside your current shell.  You can also
use Emacs in graphics mode, a GUI application that provides a graphical user
interface and ability to display images.  Each mode has its advantages and
disadvantages.  You may want to use both and activate features in a mode but
not in the other to reduce their respective startup time and memory footprint.

PEL controls activation of Emacs features through customization and stores the
customization information inside a customization file.  By default Emacs
stores the customization information as Emacs Lisp code inside the init.el
file.  PEL prevents Emacs from doing that.  Instead it instructs Emacs to
use a specific file for the customization data: the file
``~/.emacs.d/emacs-customization.el``.

In the **OPTION A** of the `example/init/init-5.el`_ file, that you should have
now copied into your own init.el, set the value of the variable
``pel-use-graphic-specific-custom-file-p`` to **t** to instruct PEL that you
want to use two independent customization files.

Then Emacs will use two files to store customization data:

- ``~/.emacs.d/emacs-customization.el`` in terminal (TTY) mode,
- ``~/.emacs.d/emacs-customization-graphics.el`` in graphics mode.

When this is activates PEL also instruct Emacs to use a different directory to
store Elpa-compliant packages: one directory will be used in terminal (TTY)
mode and another will be used for Emacs running in graphics mode.   This way
when activating an external package for one mode it will not affect the other
mode.  If you need it in both modes then you will have to activate it in both
modes.

Over time you may find the process cumbersome.  You may then want to take
advantage of Emacs built-in Ediff capabilities to show a diff of these two
customization files and copy settings from one to the other.

The easiest way to do this with PEL is to open the 2 files, each one in its
own buffer window and show only these 2 windows.  Then execute the
``pel-ediff-2files`` command by typing the PEL ``<f11> d 2`` key sequence.
Type ``?`` to display ediff help and the commands to navigate through the
files and their differences.  You can copy one set of changes from one file to
the other this way.  It's a quick way to duplicate customization and also a
good way to review the recent changes to your customization.


Add Support for Package Quickstart for Emacs 27 and Later
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs 27 introduced the **package-quickstart** user-option variable which
improves Emacs startup speed.   To use this feature you must customize the
Emacs user-option ``package-quickstart`` to **t**.

That mechanism initializes the package library before init.el is executed and
some variables like ``package-user-dir`` cannot be modified inside init.el,
they must be modified inside a new file: the file called
``~/.emacs.d/early-init.el``.

If you want to use the PEL fast startup mechanism with Emacs 27+, then you
must include the following code inside the early-init.el file:

.. code:: elisp

          ;; -*- lexical-binding: t; -*-

          ;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
          (let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                            user-emacs-directory)))
            (when (file-exists-p fast-startup-setup-fname)
              (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
              (pel-fast-startup-set-builtins)
              ;; Remember Emacs is running in PEL's fast startup mode.
              (setq pel-running-with-bundled-packages t))))


If you use only one customization file and have nothing special to configure
there then nothing else is needed in the file.

However, if you want, for example, to have 2 different customization settings,
one when Emacs runs in terminal mode, and one when Emacs is running in
graphics mode, and you want to maintain two sets of elpa directories, one for
each configuration, then you have to write the logic inside the early-init.el
file.

Here's an example of early-init.el, the `example/init/early-init.el`_ that
supports PEL fast startup and ability to use a customization in terminal mode
and another in graphics mode:

.. code:: elisp

      ;; -*- lexical-binding: t; -*-
      ;;
      ;; Emacs >= 27 support the `package-quickstart' feature which speeds-up
      ;; Emacs startup time by building the autoloads for all elpa external
      ;; packages ahead of time in a previous Emacs session.

      ;; The Emacs quick start mechanism is activated by the presence of a
      ;; early-init.el file in the user-emacs-directory.  The early-init.el
      ;; file is loaded very early in the startup process, before graphical
      ;; elements are initialized and before the package manager is
      ;; initialized.
      ;;
      ;; The following variables must be initialized in early-init.el:
      ;;
      ;; - `package-quickstart' must be set to t to activate the package
      ;;   quickstart mechanism.  Its documentation states that it can be
      ;;   customized, but the customized value is read too late in the
      ;;   process, therefore you should avoid modifying its value through
      ;;   customization.
      ;; - `package-user-dir': If you need to modify `package-user-dir' when
      ;;   the package quickstart is used in normal startup mode, then the
      ;;   value that differ from the default must be set inside early-init.el
      ;;
      ;; - `package-load-list': By default this is set to '(all) to specify
      ;;    that `package-initialize' should load the latest installed version
      ;;    of all packages. If you need to modify this behaviour when the
      ;;    package quickstart is used, set the value inside the early-init.el


      ;; Inform later code that package quickstart is being used.
      (setq package-quickstart t)

      ;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
      (let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                        user-emacs-directory)))
        (when (file-exists-p fast-startup-setup-fname)
          (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
          (pel-fast-startup-set-builtins)
          ;; Remember Emacs is running in PEL's fast startup mode.
          (setq pel-running-with-bundled-packages t)))

      ;; Init option A: independent customization for TTY & graphic modes.
      ;; Separate elpa directory for Emacs in graphics mode and Emacs in TTY mode.
      ;; Use ~/.emacs.d/elpa in TTY mode, use ~/.emacs.d/elpa-graphics in graphics mode
      ;; Inside early-init.el the function `display-graphic-p' does not return t for
      ;; Emacs running in graphics mode, so instead I use a shell script to start Emacs in
      ;; graphics mode and set the PEL_EMACS_IN_GRAPHICS environment variable to "1"
      ;; inside that shell script otherwise do not define the variable.
      ;;
      ;; To activate init option A for Emacs 27+ you must use a specialized shell
      ;; that sets the PEL_EMACS_IN_GRAPHICS environment variable for Emacs used
      ;; in graphics mode and don't set it for Emacs running in TTY mode.
      (if (getenv "PEL_EMACS_IN_GRAPHICS")
          (progn
            (setq package-user-dir (locate-user-emacs-file "elpa-graphics"))
            (setq custom-file      (expand-file-name "emacs-customization-graphics.el"
                                                     user-emacs-directory)))
        (setq custom-file (expand-file-name "emacs-customization.el"
                                            user-emacs-directory)))

      ;; ---------------------------------------------------------------------------

.. _example/init/early-init.el:               ../example/init/early-init.el

.. ---------------------------------------------------------------------------

Fast Track Installation Steps
-----------------------------

This section just lists the commands that must be used to install PEL once the
required tools are installed.  If you have not done that yet, go back to
`How to Install PEL`_.  Otherwise keep reading.

**Important**:
   If you have already used Emacs and have a ``~/.emacs.d`` and its ``init.el` file,
   you should follow the detailed instructions.  If you want to use this
   fast-track then move your ``~/.emacs.d/init.el`` file somewhere else because it
   will be deleted by the following steps.  Or move your entire ``~/.emacs.d``
   directory somewhere else.  Later you can then merge the files.


To install PEL, open a terminal shell and execute the following commands in
sequence:

#. Clone PEL repository into ``~/projects/pel``:

   .. code:: shell

          cd
          mkdir projects
          cd projects
          git clone https://github.com/pierre-rouleau/pel.git

#. Create ~/.emacs.d directory, sub-directories and required files

   .. code:: shell

          mkdir -p ~/.emacs.d/utils
          touch ~/.emacs.d/emacs-customization.el

#. Create a simple ``~/.emacs.d/init.el`` using the provided example

   .. code:: shell

          cp ~/projects/pel/example/init/init-1.el ~/.emacs.d/init.el

#. Build PEL: byte-compile all PEL source code files:

   .. code:: shell

          cd ~/projects/pel
          make clean
          make


#. Open emacs to download packages like which-key_ activated by default:

   .. code:: shell

          emacs

#. Once Emacs has completed the download, you can use Emacs with PEL.

   - Type ``<f11> ? p`` and a topic to open one of the PEL PDF files.
     Use tab to complete what you type.  Type tab at first to see a complete
     list of PDF files.
   - As usual in Emacs, type ``C-x C-c`` to close it.


At this point PEL is installed.  You should continue and perform the extra
steps to increase the performance of Emacs and PEL:

#. Setup Delay Loading of Abbreviation Definition File:

   .. code:: shell

          cp ~/projects/pel/example/init/init-2.el ~/.emacs.d/init.el
          touch ~/.emacs.d/abbrev_defs

#. Install a spell-checker program.  It must be ispell-compatible.
   Use you system installation command to install ispell, aspell, or hunspell.


#. Speed-up Emacs: hold garbage collection during startup, postpone as much as
   possible and support PEL fast-startup.

   .. code:: shell

          cp ~/projects/pel/example/init/init-5.el ~/.emacs.d/init.el

#. Edit the ``~/.emacs.d/init.el``: search for the word ``OPTION`` and
   update what is relevant for you.
   For more information read the following sections:

   - `Add Support for Fast Startup`_
   - `Add Support for Independent Customization of Graphics and Terminal based
     Emacs`_
   - `Add Support for Package Quickstart for Emacs 27 and Later`_

At this point, continue to the next section:
`Activate PEL Features - Customize PEL`_.

.. ---------------------------------------------------------------------------


Activate PEL Features - Customize PEL
-------------------------------------

Once PEL is built, you can run Emacs and select the packages you want to use by
customizing Emacs and setting the PEL user options to activate the packages you
want to use.

There are several ways to customize PEL and key sequences to access the
various customization buffers.

The easiest way at first - use the PEL customization tree
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At first, the easiest way to learn PEL customization of various features is to
use the customization browser on PEL tree.  You can then inspect each group
one by one and activate what you want to use.

To do that, type ``<f11> <f2> P B`` or type ``M-x pel-browse-pel``.
This will open the customization tree at to root of PEL.

Open another window side by side with ``C-x 3`` and select one option to
customize inside it.  You can continue to use the other window to browse the
customization tree and go over all features that PEL can activate.

Here's a screen capture of that activity:


.. image:: res/pel-start-customizing.png


Access PEL Customization Root
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Later you may want to access the PEL customization buffer from the top.
One way to quickly gain access to PEL customization group inside Emacs is to
execute the ``pel-cfg`` command by typing the ``<f11> <f2> P !`` key sequence.
When running Emacs in graphics mode, you'll see something like this:


.. image:: res/pel-cfg.png

Emacs opens a buffer in ``Custom-mode`` and shows the top level of PEL
configuration.  PEL has a large tree of customization groups, each holding
several customization user options variables.

All PEL package control user options have a name that starts with ``pel-use-``
and they are part of the ``pel-package-use`` customization group.  If you select
that group Emacs will open it and you will see something like this:

.. image:: res/pel-cfg-package.png

It shows the top level group for different types of packages, grouped by
functionality type.

If you want to see all ``pel-use-`` variables, you can also type ``pel-use-`` in
the field to the right of the **Search** button and press that button.  Emacs
will list all ``pel-use-`` user option variables by alphabetical order, as shown
below.  Set the ones you want to activate.  Then save your configuration and
restart Emacs.

The following show a lot of options **on**.  Most of them are turned
off by default when you first get PEL.  Turned them on, save the customization
and restart Emacs to activate them.  When you restart Emacs, some more packages
might be automatically downloaded when required.

Note:  In Emacs Lisp the value **t**, is the symbol for truth and **nil** is
used for the empty list and represent falsehood.

.. image:: res/pel-cfg-all-use.png

Now you are done! 😀

You can repeat the operation several times.  If you saved the customization, you
can exit Emacs: the new features will be available the next time you start it.

You can also see the following sections for some extra customization and
optimizations.

See section `PEL Use Variables`_ for more info on quickly listing all
``pel-use`` user option variables and `Key Binding Help`_ for a quick trick to
see what's available at the keyboard.

Also note that PEL includes links to the PDF *reference* sheet files relevant
to the PEL customization group.  You can open your local PDF file by clicking on
the button to the right of the "*See also*" note as shown here:

.. image:: res/pel-link-to-pdf.png


Further PEL Customization
-------------------------

The following sections describe optional optimizations or modifications
that can be done after the first complete and successful installation of PEL.


Delay Loading of Abbreviation Definition File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Description**

Emacs automatic abbreviation control provided by the ``abbrev-mode`` described
in `PEL Abbreviation Management Support`_ store all abbreviations in a file
identified by the ``abbrev-file-name`` user option variable.  By default
its value is ``~/.emacs.d/abbrev_defs``.  Emacs load the content of this file
during its initialization time.  When the file contains a large number of
abbreviations, the loading time can become annoyingly significant.

PEL provides a mechanism to delay the loading to speed up the Emacs
initialization time.  A change in the init.el file is required: read and cache
the content of ``abbrev-file-name`` user option variable and then set it to the name
of an non-existing file as early as possible in your init.el file.  Then pass
the cached value to ``pel-init`` optional argument.  By doing this you prevent
Emacs from reading the abbreviation file and let PEL load it later silently when
there is some idle time.

This code is included but commented-out in the init.el sample described in the
PEL installation section titled `Create or Update your Emacs init.el file`_.
You can also use the `example/init/init-2.el`_ file which contains the code as it
should be.  Edit your init.el file to activate the code.


**Do this:**

- Create an empty file: ``~/.emacs.d/abbrev_defs``:

  .. code:: shell

            touch ~/.emacs.d/abbrev_defs


- Modify your init.el file such that it contains the same code as the
  `example/init/init-2.el`_:

  - Write code similar to the following early at the beginning of your init.el file:

    .. code:: elisp

        (setq pel--abbrev-file-name abbrev-file-name)
        (setq abbrev-file-name "~/abbrev_defs-invalid") ; use a non-existing file name

  - Then pass the information when you call ``pel-init``:

    .. code:: elisp

        (pel-init pel--abbrev-file-name)



.. _example/init/init-2.el: ../example/init/init-2.el


Configure Spell Checking
~~~~~~~~~~~~~~~~~~~~~~~~

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

Emacs and PEL Optimizations
---------------------------

The following sections describe optimizations you can use anywhere, with or
without PEL.

Tricks to Speed-up your Emacs init time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL itself loads quickly. You can use the following tricks to speed it up further.

Holding Garbage Collection during startup
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the following code to postpone Emacs Lisp garbage collection during Emacs
startup.  Enclose the entire code of your init.el file inside the following
let form:

.. code:: elisp

          (let ((file-name-handler-alist nil)
                (gc-cons-threshold most-positive-fixnum))

            ;; all your initialization code goes here

          )

What the above does is to disable special file association handling and garbage
collection while Emacs processes your initialization code.  This has nothing to
do with PEL though.

The file `example/init/init-3.el`_ contains this logic.

.. _example/init/init-3.el: ../example/init/init-3.el


Convenience Tricks
~~~~~~~~~~~~~~~~~~

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


Generic Tips
------------

The following sections contain information related to Emacs and the OS
environment.

Extra configuration for graphics mode Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See the `example/init/init-3.el`_ for an init.el example that sets some items for
Emacs running in graphical mode.


Launching graphics mode Emacs from a shell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under Unix-like Operating Systems like Linux and macOS when you run Emacs in
graphics mode, Emacs may not get the complete environment variables that you get
in your shell.  That happens quite often in macOS as explained by
`Steve Purcel in the readme file of his exec-path-from-shell`_ package.
His package provides a way to fix the problem.

Currently PEL does not integrate that package.

To avoid the problem, I recommend using a small script that allows you to start
your graphics copy of Emacs from a shell.  There are several advantages:

- the graphical Emacs inherits the complete environment of the shell from which
  it is launched, without having to add the package written by Steve Purcell,
- you can launch several instances of graphics Emacs, from the same or different
  shells, where different shells may have different values for important
  environment variables, and that might include different versions of important
  programming languages related yo your project.


On macOS
^^^^^^^^

On macOS I use the terminal-based Emacs more often than the graphical one
because I can configure my terminal to generate ANSI sequence codes for the
functions keys, the cursor keys and the numerical keypad keys.
See the `macOS-terminal-settings PDF`_ for more information.

.. _macOS-terminal-settings PDF: pdf/macOS-terminal-settings.pdf

On macOS system I use 2 commands that launch Emacs:

- ``e`` which launches a Termcap (TTY) character-only version of Emacs, and
- ``ge`` which launches the GUI version of Emacs.

The ``e`` command is just a symlink to the Termcap Emacs version I am currently
using.  I use this short symlink for its size. And you probably don't have to
worry about a clash with the `1970s E editor`_.

And ``ge`` is a script to launch the graphical Emacs, providing access to the
current directory and the complete environment on macOS.  With that script I
don't need any special Emacs setup code like `Steve Purcell's exec-path-from-shell`_:


.. code:: shell

          #!/bin/sh
          # Abstract: open Cocoa-based GUI Emacs in the background
          #           (so we can continue using the shell).
          # Pass to emacs:
          #   - --chdir to the current working directory so we open the same files
          #     as what is specified on the command line. If we don't do that the GUI
          #     based Emacs might use a different directory (I saw that it uses the home
          #     directory) and if you specify files that are not in that directory they
          #     will not be opened, another file file open which will most likely be
          #     in an empty buffer (if the file does not exists in the home directory).
          #   - All script command line arguments
          #
          # Note: The current Emacs for macOS graphical dumps an error when it starts.
          #       This is annoying; it's noise on the shell.
          #       Just mask it by dumping it in the bit bucket.
          #
          # Emacs 27+ support:
          #    - To allow Emacs early-init.el code to distinguish whether Emacs is
          #      running in terminal mode or in graphics mode.  When running
          #           early-init.el Emacs does not know and the function
          #           display-graphic-p does not work at that moment.  The only way I
          #           have found is to use an environment variable.  So the following
          #           code sets one up: PEL_EMACS_IN_GRAPHICS
          # See: https://emacs.stackexchange.com/questions/66268/how-to-set-package-user-dir-with-emacs-27-with-package-quickstart-and-distinguis
          #
          #
          export PEL_EMACS_IN_GRAPHICS=1
          /Applications/Emacs.app/Contents/MacOS/Emacs --chdir=$(pwd) "$@" 2>/dev/null &


.. _Steve Purcell's exec-path-from-shell: https://github.com/purcell/exec-path-from-shell



On Linux
^^^^^^^^

For Linux, if you do not want to store a new executable script in a directory
on your PATH, or create a new directory to your PATH, you can create a
directory like ``~/bin`` that will hold a script and use a ``.bashrc`` alias
to create a simple command to access it.  This way you don't add anything to
your PATH.

Add the following to your ``~/.bashrc`` file :

.. code:: shell

          # Additions to you .bashrc for short Emacs commands line
          #
          # Add the following alias to your .bashrc
          #
          #  - ee:  starts Emacs in terminal mode.
          #        NOTE: unless your terminal implements ANSI code sequences
          #              for the cursor keys and the function keys you won't be able
          #              to use PEL main key prefixes.
          #
          #  - e:  start Emacs in graphical mode.
          #         Use the bash executable script graphics-emacs.sh stored in the
          #         ~/bin directory.
          #         Copy the pel/example/bin/graphics-emacs.sh in a ~/bin directory,
          #         make the file executable (chmod +x) and use the following alias.
          #         This way you do not have to add ~/bin to your PATH.

          alias ee='emacs -nw'
          alias e='~/bin/graphics-emacs.sh'

Then store the following script inside the file ``~/bin/graphics-emacs.sh``:

.. code:: shell

        #!/bin/sh
        #  SH FILE: graphics-emacs.sh
        #
        #  Purpose   : Run graphics Emacs asynchronously on specified files.
        #  Created   : Tuesday, September 29 2020.
        #  Author    : Pierre Rouleau <prouleau001@gmail.com>
        #  Time-stamp: <2020-09-29 22:28:41, updated by Pierre Rouleau>
        # --------------------------------------------------------------------
        emacs --chdir=$(pwd) "$@" 2>/dev/null &
        # --------------------------------------------------------------------


With these you will be able to open any file(s) with Emacs from the command
line, doing something like this:

.. code:: shell

          e hello.c
          e hello.c hello.h
          e *.c

.. _Steve Purcel in the readme file of his exec-path-from-shell: https://github.com/purcell/exec-path-from-shell#setting-up-your-shell-startup-files-correctly
.. _1970s E editor: https://en.wikipedia.org/wiki/E_(1970s_text_editor)
.. _example/init/init-3.el: ../example/init/init-3.el

Using benchmark-init
~~~~~~~~~~~~~~~~~~~~

If you want to know the time each loaded file takes during Emacs initialization
time you can use the benchmark-init_ package. This is not controlled by PEL
because it must be launched as as early as possible inside your init.el file.

To install it type ``M-x list-packages`` then hit the return key to get a list
of all elpa-compliant packages. Search for ``benchmark-init``, select it and
install it.  You can also type: ``M-x package-install benchmark-init``.

Then add the following code as close as possible to the top of your init.el file:

.. code:: elisp

  (require 'benchmark-init
           (expand-file-name
            "~/.emacs.d/elpa/benchmark-init-20150905.938/benchmark-init"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

This code is inside the file `example/init/init-4.el`_.

With the above code in your init.el file, you can then execute the PEL command
``pel-show-init-time`` (or using the ``<M-S-f9>`` keystroke for it) Emacs will
open 2 buffers and will show something like this:

.. image:: res/pel-benchmark.png

This is a snapshot taken on GNU Emacs running in terminal mode on a 2014 macOS
computer with PEL running with 96 packages selected by customization giving 156
lines inside the benchmark-init buffers.

Here's another snapshot taken after installing PEL on Mint 20 Linux running
inside Parallels Desktop VM under macOS host:

.. image:: res/pel-benchmark-mint20.png


.. _example/init/init-4.el: ../example/init/init-4.el


Disable Emacs Startup splash screen and echo area message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default Emacs displays its splash screen on startup and displays a message on
the echo area telling you about Emacs in general and the concept of free
software. Once you have read this information, you can disable them with the
following code:

.. code:: elisp

  ;; Do not display the splash screen.  Same as emacs -Q
  (setq inhibit-startup-screen t)
  ;; Don't display the start help in minibuffer, at least for me.
  (setq inhibit-startup-echo-area-message "YOUR-USER_NAME_HERE")

Replace "YOUR_USER_NAME_HERE" by a string containing your user name.
Emacs was written to allow multiple users from having access to the same
configuration, and this identifies the user that will not be reminded of Emacs
concepts and principles every time Emacs starts.  So, to take advantage of that
small speed up make sure you put your user name there.

The file `example/init/init-5.el`_ contains the code that disables the splash
screen. the code that disable the message is still commented out.

.. _example/init/init-5.el: ../example/init/init-5.el

To override or change PEL key bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


More Emacs Customization
------------------------

If this is the first time you use Emacs you will also want to customize the
following options.  Use ``<f11> <f2> o`` or ``M-x customize-option`` for each
of those.

======================================== ======================================
Emacs user option                        Description
======================================== ======================================
bookmark-save-flag                       Set it to **1** to get Emacs to save
                                         the bookmarks inside the bookmark
                                         file every time a bookmark is set.

bookmark-default-file                    Set the location of the bookmark
                                         file. Something like
                                         ``~/.emacs.d/bookmarks``

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

imenu-max-items                          Set the maximum number of entries in
                                         the imenu list if the default of 25
                                         does not correspond to what you like.

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


.. ---------------------------------------------------------------------------

Get Emacs To Startup FAST
=========================

Once you have installed and configured all external packages you need you may
find that Emacs startup time has increased too much for your liking.  That
will be the case if you use a large number of external Elpa-compliant packages
because Emacs must process the autoloads package information inside each of
the elpa directory and that takes time.  There are several techniques that can
be used to speed this up and PEL uses a large number of these techniques when
it operates in the normal operation mode.

To experience a faster Emacs init startup you can use PEL's fast-startup
operation mode.

PEL provides the following 3 commands:

- **pel-setup-info**, bound to ``<f11> M-S ?``.  It displays the current
  operation mode.
- **pel-setup-fast**, bound to ``<f11> M-S f``.  This commands reorganizes
  the content of your ``user-emacs-directory`` to bundle the elpa external
  packages to provide a faster Emacs init startup time.  In this mode you
  cannot add new external packages though.
- **pel-setup-normal**, bound to ``<f11> M-S n``.  This command restores
  the content of your ``user-emacs-directory`` the way it was, allowing you to
  use Emacs as before and with the ability to add or remove external packages.

These commands are described in the `Fast Startup PDF Sheet`_.

The speedup you will experience depends on several factors:

- One main factor is the number of Elpa-compliant packages that PEL can
  bundle.  PEL will be able to bundle all those packages that put all their
  files inside a single directory.  PEL will then build a single
  pel-bundle-autoloads.el file and one pel-bundle-pkg.el for all of these
  packages.  By doing so, and by adding extra code to make the whole thing
  work, by delaying package initialization in the init.el file, PEL reduces
  Emacs-load path and overall startup processing.

- Another significant factor is the init.el code. The execution of
  ``package-init`` must be delayed.  See the file `example/init/init-5.el`_
  for a setup that properly supports PEL fast-startup.

It's possible to reduce the startup time down such that benchmark-init report
it to be 0.1 second, even with a relatively large number of external package.

On my 2014 iMac computer with 4GHz Intel Core i7 and Flash storage memory,
running macOs Mojave and Emacs 26.3 in terminal mode, I was able to get Emacs
startup in 0.1 second with 238 external packages.

In the following screen shots you can see the benchmark-init report for the
same setup when normal mode is used and then when PEL fast-startup operation
mode is used.

Screen Shot #1: Emacs 26.3 in terminal mode using 238 external packages in
normal mode exhibiting a 0.6 second startup:

.. image::  res/normal-startup-001.png

Screen Shot #2: With the same setup as above but now running under PEL's
fast-startup operation mode: Emacs startup time is now around 0.1 second.

.. image::  res/fast-startup-001.png





.. _Fast Startup PDF Sheet: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/fast-startup.pdf


.. -----------------------------------------------------------------------------

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
before the ``make``.

Then to use a new feature, set the PEL activation user variable through
the relevant Emacs customization buffer, save the new customization data and run
pel-init by using ``M-x pel-init`` or restart Emacs.  If new packages are
identified by your new configuration they will be downloaded, installed and
byte-compiled automatically.  You'll be able to activate them via the new
``pel-use-`` user options.

.. -----------------------------------------------------------------------------

Key Binding Help
================


By default, PEL configuration activates the which-key_ external package so that
when you hit a key prefix, like **F11** the list of comments and their key
bindings will show up at the bottom of the Emacs screen, in what is called the
echo area.  This, like everything PEL uses, can be turned off by customization
(in this case it's the user option called ``pel-use-which-key``.

You can also see the list of commands without it. For example,
you can see PEL's use of the **F11** function key by hitting in sequence the
**F11** key quickly followed by the **F1** key.  Emacs will list PEL's **F11**
key bindings inside the ``*Help*`` buffer.

You can also open the local copy of the  PDF *reference* sheet file that
describes the commands and key bindings accessible through a given key prefix by
using the **F1** key inside that key prefix.
For example, as described in section `PEL Help Support`_, the PEL key prefix
for help and information commands is **pel:help** bound to the ``<f11> ?`` key
sequence.  To open the `HELP`_ PDF file, type ``<f11> ? <f1>``.
Not all PEL key prefixes have this key, but most have.


.. -----------------------------------------------------------------------------

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
- ``pel-completion-help`` shows the state of the auto completion global and
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

**Note:**
   🚧  This file is under early development.

The file `pel-xref.el`_ holds utilities related to Etags based cross-reference support.


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
:PEL Customization: *none*
:PEL Key Prefix: - **pel:diff** : ``<f11> d``
                 - **pel:ediff** : ``<f11> e``

                   - **pel:ediff-buffer**  : ``<f11> e b``
                   - **pel:ediff-dirs**    : ``<f11> e d``
                   - **pel:ediff-files**   : ``<f11> e f``
                   - **pel:ediff-merge**   : ``<f11> e m``
                   - **pel:ediff-patch**   : ``<f11> e p``
                   - **pel:ediff-regions** : ``<f11> e r``

PEL provides key bindings to Emacs diff end ediff commands.

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

.. _Hydra keys: https://github.com/abo-abo/hydra


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
save the file back and restart Emacs.  If you followed the instructions in
section titled `Create the emacs-customization.el file`_, this name of this file
is "``~/.emacs.d/emacs-customization.el``".

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

PEL Programming Language Support
--------------------------------

PEL programming language support assigns the **F12** key as the prefix key for
the programming language.  The prefix key is the same for other programming
languages (or markup languages) but the key bindings after the prefix differ,
while keeping as similar keys as possible.

Note:
  PEL support for programming languages is currently embryonic in this early
  version of PEL.
  It will be enhanced with upcoming versions.

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


PEL Support For LISP-based Languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL provides explicit support for the following
`LISP-based programming languages`_:

- `Common Lisp <https://en.wikipedia.org/wiki/Common_Lisp>`_
- `Emacs Lisp  <https://en.wikipedia.org/wiki/Emacs_Lisp>`_

.. _LISP-based programming languages: https://en.wikipedia.org/wiki/Lisp_(programming_language)

PEL Support for Common Lisp
^^^^^^^^^^^^^^^^^^^^^^^^^^^

:PDF Sheet: `Common Lisp <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-common-lisp.pdf>`_.
:PEL Customization: - Group: ``pel-pkg-for-clisp``

                      - Activation:    ``pel-use-common-lisp``.

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


PEL Support for Emacs Lisp
^^^^^^^^^^^^^^^^^^^^^^^^^^

:PDF Sheet: `Emacs Lisp <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-emacs-lisp.pdf>`_, `ERT`_ .
:PEL Customization: - Group: ``pel-pkg-for-elisp``

                      - Activation: (*none* to use Emacs Lisp), but there are
                        for other packages:

                        - ``pel-use-macrostep``
                        - ``pel-use-esup``
                        - ``pel-use-re-builder``
                        - ``pel-use-highlight-defined``

:PEL Key Prefix: - Globally: **pel:for-elisp** : ``<f11> SPC l``
                 - From a buffer in elisp-mode: ``<f12>`` and ``<M-f12>``


The file `pel-lisp.el`_ contains command utilities that help edit Emacs Lisp
code.  Some of them can also be used for other types of Lisp as well.

- ``pel-toggle-lisp-modes`` toggles between ``lisp-interaction-mode`` and
  ``emacs-lisp-mode``.
- ``pel-byte-compile-file-and-load`` byte compiles the file in the current
  buffer and then load it.
- ``pel-lint-elisp-file`` runs Emacs Lisp lint on the current file.


PEL Support for BEAM-VM Programming Languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Note:**
   🚧  This file is under early development.

PEL will support several BEAM programming languages, starting with:

- Erlang
- Elixir
- LFE (Lisp Flavoured Erlang)

For the moment support for Erlang is the most evolved of those but more work is
required..  Support for the others is minimal for the moment.


PEL Support for Erlang
^^^^^^^^^^^^^^^^^^^^^^

:PDF Sheet: `Erlang <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-erlang``.  Use ``<f21> <f1>`` from and erlang mode buffer.

                      - Activation:

                        - ``pel-use-erlang``
                        - ``pel-use-edts``
                        - ``pel-use-erlang-flycheck``
                        - ``pel-use-erlang-flymake``

:PEL Key Prefix: - Globally: **pel:for-erlang** : ``<f11> SPC e``
                 - From a buffer in erlang-mode: ``<f12>`` and ``<M-f12>``

PEL provides support for the Erlang Programming Language via the Erlang official
Emacs support, the integration of several Emacs library supporting Erlang and
some PEL code.  PEL provides access to the Tempo skeleton and yasnippet_
template text insertion systems.  PEL adds functionality to several of the
Erlang skeletons, provides the ability to select several commenting styles via
user option variables that can be customized (use the ``<f12> <f2>`` key from a
buffer in erlang major mode to quickly gain access to the buffer to see and/or
change those variables).
Refer to the `PEL Erlang PDF`_ document for more information.

.. _PEL Erlang PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf

PEL Support for Elixir
^^^^^^^^^^^^^^^^^^^^^^

:PDF Sheet: `Elixir <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-elixir.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-elixir``

                      - Activation: ``pel-use-elixir``

:PEL Key Prefix: - Globally: **pel:for-elixir** : ``<f11> SPC x``
                 - From a buffer in elixir-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Elixir programming language`_ via the
elixir-mode_ package.  With it the file extensions ``.exs``, ``.ex``, and
``.elixir`` are automatically recognized as being Elixir files.

.. _Elixir programming language: https://en.wikipedia.org/wiki/Elixir_(programming_language)

PEL Support for Curly-Bracket Programming Languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL provides explicit support for the following
`curly-bracket programming languages`_:

- `C  <https://en.wikipedia.org/wiki/C_(programming_language)>`_
- `C++ <https://en.wikipedia.org/wiki/C%2B%2B>`_
- `D <https://en.wikipedia.org/wiki/D_(programming_language)>`_

.. _curly-bracket programming languages: https://en.wikipedia.org/wiki/List_of_programming_languages_by_type#Curly-bracket_languages

PEL Support For C
^^^^^^^^^^^^^^^^^

:PDF Sheet: `C <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-c``

                      - Activation: *none*
                      - Configuration:

                        - ``pel-c-indent-width``
                        - ``pel-c-tab-width``
                        - ``pel-c-use-tabs``
                        - ``pel-c-bracket-style``

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
+++++++++++++++

Ideally the editor should support various indentation styles for the C
preprocessor directives.  A selection of styles is shown in the next section.

I am working to implement the ability to support all of these styles.
But it is work in progress.
I will update this section once this work is done.


Indentation of C pre-processor statements
*****************************************

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
++++++++++++++++

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
****************************************

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
*************************************

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
***********************************

To insert a C file module/header block inside a C file, use
the ``<f12> <f12> h`` key sequence.  It will prompt for the purpose of the
file.  Once you hit the return key it inserts the comment block at the
beginning of the C file according to the user options that are part of the
``pel-c-code-style`` customization group.  See the example in the next section.

In a .c C file
>>>>>>>>>>>>>>

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
                                             "Module Description"
                                             "Header Inclusion", "Local Types",
                                             "Local Variables" and "Code".
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
>>>>>>>>>>>>>>>>>>

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
*******************

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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



PEL Support For C++
^^^^^^^^^^^^^^^^^^^

:PDF Sheet: `C++ <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c++.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-c++``

                      - Activation: *none*
                      - Configuration:

                        - ``pel-c++-indent-width``
                        - ``pel-c++-tab-width``
                        - ``pel-c++-use-tabs``
                        - ``pel-c++-bracket-style``

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

PEL Support For D
^^^^^^^^^^^^^^^^^

:PDF Sheet: `D <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-d.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-d``

                      - Activation: ``pel-use-d``
                      - Configuration:

                        - ``pel-d-indent-width``
                        - ``pel-d-tab-width``
                        - ``pel-d-use-tabs``
                        - ``pel-d-bracket-style``

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

PEL Support for Forth
~~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `Forth <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-forth.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-forth``

                      - Activation: ``pel-use-forth``

:PEL Key Prefix: - Globally: **pel:for-forth** : ``<f11> SPC f``
                 - From a buffer in forth-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Forth programming language`_ via the
forth-mode_ package.  With it the file extensions ``.f``, ``.fs``, ``.fth``, and
``.4th`` are automatically recognized as being Forth files.

.. _Forth programming language: https://en.wikipedia.org/wiki/Forth_(programming_language)

PEL Support for Julia
~~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `Julia <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-julia.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-julia``

                      - Activation: ``pel-use-julia``

:PEL Key Prefix: - Globally: **pel:for-julia** : ``<f11> SPC j``
                 - From a buffer in julia-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Julia programming language`_ via the
julia-mode_ package.  With it the file extensions ``.jl``
are automatically recognized as being Julia files.

.. Julia programming language: https://en.wikipedia.org/wiki/Julia_(programming_language)

PEL Support for Python
~~~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `Python <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-python.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-python``

                      - Activation: ``pel-use-python``

:PEL Key Prefix: - Globally: **pel:for-python** : ``<f11> SPC p``
                 - From a buffer in python-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Python programming language`_ via the
python-mode_ package.  With it the file extensions ``.py``, ``.pyi`` and ``.pyw``
are automatically recognized as being Python files.

.. _Python programming language: https://en.wikipedia.org/wiki/Python_(programming_language)


PEL Support for REXX
~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `REXX <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-rexx.pdf>`_
:PEL Customization: - Group: ``pel-pkg-for-rexx``

                      - Activation: ``pel-use-rexx``

:PEL Key Prefix: - Globally: **pel:for-forth** : ``<f11> SPC R``
                 - From a buffer in rexx-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `REXX programming language`_ via the
forth-mode_ package.  With it the file extensions ``.rexx``, ``.elx``, ``.ncomm``, and
``.cpr`` are automatically recognized as being REXX files.

.. _REXX programming language: https://en.wikipedia.org/wiki/Rexx


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
:PEL Customization: - ``pel-use-lice``.
                    - ``pel-use-smart-dash``
                    - ``pel-use-yasnippet``
                    - ``pel-use-yasnippet-snippets``
                    - **pel-c-code-style-group**
                    - **pel-elisp-code-style-group**
                    - **pel-erlang-code-style-group**

:PEL Key Prefix: **pel:insert** : ``<f11> i``
                 **pel:yasnippet** : ``<f11> y``


The file `pel-text-insert.el`_ provides a few commands to insert some text
quickly.  PEL does not yet integrate the support of one or several of the great
template systems that are available for Emacs, for now it just provides the
following commands:

- ``pel-insert-line`` inserts a (commented) line.  The length of the line is
  controlled by the ``pel-linelen`` customization variable, which defaults to 77.
- ``pel-insert-filename`` inserts the name of the file in the current or
  specified window.
- The following 3 commands insert time/date format for the local or the UTC
  time:

  - ``pel-insert-current-date-time`` inserts the current date and time at point.
  - ``pel-insert-current-date`` inserts the current date at point.
  - ``pel-insert-iso8601-timestamp`` inserts a ISO 8601 conforming date and time
    string.

Another **very useful** feature is the use of the ``smart-dash-mode`` provided
by the smart-dash_ external package.  PEL provides the ``<f11> M--`` binding to
toggle this useful mode on and off. When the ``smart-dash-mode`` is activated,
you can insert underscore characters by hitting the dash (``'-'``) key without
having to press the Shift key.   And for programming languages identified by the
``smart-dash-c-modes`` user option you can insert ``--`` and ``->`` normally.

The PEL binding include more commands, some are Emacs standard commands, other
are from other packages.  All are listed in the `Inserting Text`_ PDF
documentation.

Template Text Insertion
~~~~~~~~~~~~~~~~~~~~~~~

PEL supports two different template mechanisms: the Emacs built-in tempo skeleton
system and the popular yasnippet_ external library.

Using Tempo Skeleton
^^^^^^^^^^^^^^^^^^^^

PEL implements tempo skeletons for several major modes:

- c,
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

  - ``pel-show-window-dedicated-status`` displays the dedicated status of the
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
  - ``pel-show-window-sizes`` displays the height and width of the current
    window.


..
   -----------------------------------------------------------------------------


PEL Key Bindings
================

PEL key bindings are mostly use function key prefixes.
It currently uses the **F2**, **F6**, **F7**, **F8**, **F11** and **F12** keys
as prefix keys.
It also binds **F5** as the repeat key.

PEL's main global prefix key is the **F11** key.  The **F12** key is also
available in some major modes as described in the section titled
`PEL Mode Sensitive Key-maps`_.

All PEL key prefixes  are used for PEL key maps.  These all have names that
start with ``pel:``.  The complete list of the prefixes is available inside the
`PEL Key Maps PDF file`_.


.. _PEL Key Maps PDF file: https://github.com/pierre-rouleau/pel/blob/master/doc/pdf/-pel-key-maps.pdf


To see the list of PEL prefix keys inside Emacs, do one of the following:

- Type the  prefix key (like **F11**) and then quickly **C-h**.  Emacs will open
  a ``*Help*`` buffer and list the available keys under F11.
- To open a local copy of the `PEL Key Maps PDF file`_ type **F11** followed by
  **F1**.

As described in the `Naming Conventions`_ section the names in the binding
column that use the ``pel:`` prefix are sub key-maps.
All PEL command functions use the prefix ``pel-``.


PEL Mode Sensitive Key-maps
---------------------------

The first element of the table in the previous section lists
the ``<f11> SPC`` special prefix.
It's the top key-map of all PEL mode sensitive key-maps.
It has several sub-maps, once for each of the major mode explicitly supported by
PEL:

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

The above list is small.  It will grow as PEL evolves.

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

These tables are inside PDF documents.  They are listed in the `PDF Document
Tables`_ below.

Open PEL PDF files quickly from Emacs:
   PEL provides a set of key bindings that open you local copy of
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

PDF Document Tables
~~~~~~~~~~~~~~~~~~~

PEL provides **over 130 PDF topics oriented reference sheets** with
hyperlinks to Emacs manuals, external packages, articles and other
references.

- The `PEL Index PDF`_ lists them all and provides a good starting
  point.
- For the best user experience, use a browser, like Firefox version 78 or later, that
  can render the PDF file inline (as opposed to downloading it) so you can use
  the links quickly.
- The hyperlinks are not active when the files are viewed through the
  Github PDF file rendering (for example, the `Github rendering of the Buffers PDF`_)
  which can also be viewed inside PEL's `doc/pdf github directory`_.
- Inside Emacs use ``<f11> <f1>`` to open the `PEL Index PDF`_ and then
  navigate from it, or use ``<f12> <f1>`` to open the PDF describing the
  major mode of the current buffer and its key bindings.
- All of these PDF files are created from a macOS Number file.  It is located
  in a separate Git repo: the `pel-pdf-spreadsheet repo`_.

.. _doc/pdf github directory: https://github.com/pierre-rouleau/pel/tree/master/doc/pdf
.. _pel-pdf-spreadsheet repo:   https://github.com/pierre-rouleau/pel-pdf-spreadsheet#readme


- `Document Legend`_

**Emacs Concepts**

#. `PEL Key Maps`_
#. `File and Directory Local Variables`_
#. `Modifier Keys`_

**Basic Emacs Operations:**

#. `Abbreviations`_
#. `Align`_
#. `Auto-Completion`_
#. `Autosave & Backups`_
#. `Bookmarks`_
#. `Buffers`_
#. `Case Conversion`_
#. `Closing and Suspending`_
#. `Comments`_
#. Completion: `Input Completion`_
#. `Counting`_
#. `CUA Mode Support`_
#. `Cross-Referencing`_
#. `Cursor`_
#. `Customization <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/customize.pdf>`_.
#. `Cut, Delete, Copy and Paste`_ (killing and yanking)
#. `Diff and Merge`_
#. `Dired`_ (Directory Editing)
#. `Display Lines`_
#. `Drawing`_
#. `Enriched Text`_
#. `Faces and Fonts`_
#. `Fast Startup`_
#. `File Management`_
#. `File and Directory Local Variables`_
#. `Filling and Justification`_
#. `Frames`_
#. `Grep`_
#. `Help`_
#. `Hide/Show Code Block`_
#. `Highlight`_
#. `ibuffer-mode`_
#. `Indentation`_
#. `Input Method`_
#. `Inserting Text`_
#. `Key Chords`_
#. `Keyboard Macros`_
#. `Lispy mode support`_
#. `Marking`_
#. `Menus`_
#. `Mode Line PDF`_
#. `Mouse`_
#. `Narrowing`_
#. `Navigation`_
#. `Number Keypad`_
#. `Outline`_
#. `Packages`_
#. `Projectile Project Interaction Manager`_
#. `Rectangles`_
#. `Registers`_
#. `Scrolling`_
#. `Search and Replace`_
#. `Sessions`_
#. `Shells`_
#. `Sorting`_
#. `Speedbar`_
#. `Spell Checking`_
#. `Syntax Check`_
#. `Text-modes`_
#. Templates_
#. `Transpose`_
#. Treemacs_
#. `Undo, Redo, Repeat and Prefix Arguments`_
#. `Web`_
#. `Whitespaces`_
#. `Windows`_

**Emacs Lisp Concepts and Tools**

#. `ERT`_
#. `Hooks`_
#. `Emacs Lisp Types`_


**Text Markup Support:**

#. `AsciiDoc <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/asciidoc.pdf>`_
#. `Graphviz Dot`_
#. `Org mode`_
#. `PlantUML-Mode`_
#. `reStructuredText mode`_

**Programming Language Support:**

#. `Apple-Script`_
#. Curly-bracket Languages

   - `C <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c.pdf>`_
   - `C++ <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c++.pdf>`_
   - `D <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-d.pdf>`_

#. BEAM-VM Languages

   - `Erlang <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf>`_
   - `Elixir <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-elixir.pdf>`_
   - LFE (🚧 under development)

#. Lisp Languages

   - `Common Lisp <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-common-lisp.pdf>`_
   - `Emacs Lisp <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-emacs-lisp.pdf>`_

     - `ERT`_ (Emacs Lisp Regression Testing system)

#. `Forth <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-forth.pdf>`_
#. `Julia <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-julia.pdf>`_
#. `Python <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-python.pdf>`_
#. `REXX <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-rexx.pdf>`_


**Version Control Systems:**

#. `Mercurial`_

**Miscellaneous**

#. `Function Keys`_
#. `F11 Keys`_
#. `macOS Terminal settings`_

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
.. _Help:                                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/help.pdf
.. _Hide/Show Code Block:                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/hide-show-code.pdf
.. _Highlight:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/highlight.pdf
.. _Hooks:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/hooks.pdf
.. _Indentation:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/indentation.pdf
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
.. _Syntax Check:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/syntax-checking.pdf
.. _Templates:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/templates.pdf
.. _Text-modes:                               https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/text-modes.pdf
.. _Transpose:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/transpose.pdf
.. _Treemacs:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/treemacs.pdf
.. _Undo, Redo, Repeat and Prefix Arguments:  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/undo-redo-repeat.pdf
.. _User Option Customization:                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/customize.pdf
.. _Web:                                      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/web.pdf
.. _Whitespaces:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/whitespaces.pdf
.. _Windows:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/windows.pdf



.. _ERT:                                      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/ert.pdf
.. _ibuffer-mode:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/ibuffer-mode.pdf
.. _macOS Terminal settings:                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/macOS-terminal-settings.pdf

.. _Org mode:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-org-mode.pdf
.. _reStructuredText mode:                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-rst.pdf
.. _Modifier Keys:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/modifier-keys.pdf
.. _Apple-Script:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-applescript.pdf
..  C++
..  C
..  Common Lisp
.. _Emacs Lisp Types:                         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/emacs-lisp-types.pdf
.. _Lispy mode support:                       https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/plm-lispy.pdf
..  D
..  elixir
..  Emacs Lisp
..  erlang
..  forth
..  python
..  REXX
.. _PlantUML-Mode:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/plantuml.pdf



PEL Customization
=================

PEL is customized by using the `Emacs easy customization`_ system.
PEL controls the activation of external packages and their key bindings
via a set of customize
variables that have names that start with ``pel-use-``.  They are listed in the
next section.

To customize PEL:

#. Decide where you want to store the persistent customization information.

   - By default it is stored inside your Emacs init file.
     If this is good for you, then continue to next step.
   - You may want to store it inside a separate file, to decouple it from your
     Emacs initialization if you use several environments or computers and
     even allow the use of *several* customization files selected by your init.el
     logic based on some criteria you may have, keeping these configurations
     isolated from each other.

     For example if your Emacs initialization file is
     ``"~/.emacs.d/init.el"`` you may want to store the customization
     inside the same directory and place it in
     ``"~/.emacs.d/emacs-customization.el"``.
     To do so add the following Emacs Lisp code inside your
     init.el file:

     .. code:: elisp

               (setq custom-file "~/.emacs.d/emacs-customization.el")
               (load custom-file)

#. If you want PEL to start automatically when Emacs starts, then add
   the following code, which must be located **after** the code
   loading the customization data (if any):

   .. code:: elisp

             (require 'pel)
             (pel-init)

   - With the above code, PEL will start when Emacs starts and do the following:

     - It will activate its main key bindings using the **F2**, **F5**, **F6**,
       **F7** (if ``pel-use-hydra`` is set to **t**), **F11** and **F12** keys.
       See the `PEL Key Bindings`_ section for more info.
     - It will **not** download or activate any other package.

       - It will only do that if you change PEL's customization and re-run
         ``pel-init`` either manually or by restarting Emacs.

#. Once the location of the customization information is identified,
   that you have decided whether to have PEL started automatically
   or not, just start Emacs.
#. Customizing PEL depends on whether ``pel-init`` was run:

   - If you ``pel-init`` was already executed, go to next step.
   - If you want to play it safe and did not yet run ``pel-init``
     then you must load pel-options:

     - type the following: ``M-x load-library``.
     - at the prompt, type: ``pel-options`` and hit the return key.

#. Execute the Emacs customize command by typing: ``M-x customize``
#. This will open the ``*Customize Apropos*`` buffer.
#. Inside that buffer, move point to the search field and
   search for the Pel group by typing ``Pel$`` inside the search
   field and hitting the Search button.
#. Emacs will show the *Pel Group*.

   - Currently, the *Pel group* has the following subgroups:

     - *Pel Identification*
     - *Pel Kbmacro*
     - *Pel Package Use*
     - *Pel Text Insert*

   To select the packages you want PEL to use select the *Pel Package Use*
   subgroup.
   This is the root of another set of subgroups, organized by topics.
   These define a set of customization variables that activate the features either
   provided by PEL code or provided by other packages which PEL uses.
   All of these variables have a name that begin with the ``pel-use-`` prefix.
   The list of these variables is available below in `PEL Use Variables`_.

#. Select the *Pel Package Use* subgroup, then the subgroup that interests you
   and activate the feature that you want to use by setting the corresponding
   ``pel-use-`` variable to **t**.
#. Save and apply you new settings.
#. Restart PEL by either executing ``M-x pel-init`` or by restarting Emacs and
   then executing ``M-x pel-init`` (unless it is already executed in you Emacs
   init file).

**A Faster Way**

Once PEL is properly installed, there is a much quicker way to do all of this:

- Use use the keys identified in the section
  `PEL Configuration/Customization Support`_ to configure any
  of the Emacs and PEL features.
- Then execute ``pel-init`` or restart Emacs.




PEL Use Variables
-----------------

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

.. image:: res/pel-use-completion.png

If you search ``pel-use-`` in a customization buffer, Emacs will also list all
corresponding user options in alphabetical order.

.. image:: res/pel-use-cfg.png

Also see the `Activate PEL Features - Customize PEL`_ section.


.. _Elixir programming language: https://en.wikipedia.org/wiki/Elixir_(programming_language)
.. _Julia Programming language:  https://en.wikipedia.org/wiki/Julia_(programming_language)
.. _LFE (Lisp Flavored Erlang) programming language: https://en.wikipedia.org/wiki/LFE_(programming_language)
.. _Emacs-libvterm vterm: https://github.com/akermu/emacs-libvterm



PEL Implementation
==================

Emacs Lisp Files
----------------

PEL code is spread across several Emacs Lisp files.
The file names have been selected with the following constraints:

#. Conform to the `Emacs Lisp Packaging rules`_ and include the
   following files:

   - `pel-pkg.el`_ that identified the project name, URL, author,
     version and dependencies.
   - `pel-autoloads.el`_ identifies the command ``pel-init`` as the
     only auto-loaded command.

#. Control byte-compilation under several scenarios, including the
   following:

   - Installation with `Emacs package-install`_ where all files are byte-compiled
     in order of their file names (in alphabetical order).
   - *Manual* installation by cloning the PEL Git Depot and then using
     the PEL Makefile_ to create a local package archive, and compile all files
     locally.

The PEL Emacs Lisp files types are the following:

#. Local files, used by all other PEL files.

   - These files have a name that
     starts with ``pel--`` and sort before all other files.
   - These files can be byte-compiled independently within an ``emacs -Q``
     session and will not generate any warning.
   - These include:

     - `pel--options.el`_: defines all PEL customization variables.
     - `pel--macros.el`_: defines macros used by other files.
     - `pel--base.el`_: defines low level utilities.

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

   - The file `build-pel.el`_ controls byte compilation of files in a specific
     order.
   - The file `install-pel.el`_ controls the creation of a local Emacs package
     archive which is then used to install PEL on local computers from a cloned
     Git depot.

PEL loads only what is needed based on commands executed.
It implements a 2-step auto-loading mechanism described here:

- At first the only PEL symbol defined is ``pel-init``: it is activated
  by `pel-autoloads.el`_ which schedule the auto-loading of `pel.el`_ when the
  ``pel-init`` command is issued (or when called by your Emacs initialization file.)
- The ``pel-init`` command loads `pel_keys.el`_ explicitly.
- The `pel_keys.el`_ code
  loads `pel-autoload.el`_ and then calls ``pel--autoload-init``.
  That function defines the auto-loading of all ``pel-``
  files, the PEL feature which are mostly independent from each other.

Currently, PEL use its own logic on top of the built-in package library
to control the installation of missing package if the corresponding feature
is activated via `PEL customization`_ ``pel-use-`` customization variable.
However, when using `Emacs package-install`_ to install PEL, then all
dependencies identified by the `pel-pkg.el`_ file will be installed as well.
They will be located inside your Emacs load-path but will only be loaded if the
corresponding ``pel-use-`` user option is set to **t**.

Note that this mechanism only works for external packages that are available from an
Elpa compatible Emacs package archive site (ELPA_, MELPA_, a local Elpa archive,
etc...)
Some of the packages PEL uses are not hosted on these sites (yet) but on site
like EmacsWiki_ and Emacsmirror_.
For the moment those packages must be installed manually.
Sites hat are accessible securely are preferred.
The list of external packages used by PEL is shown in the `Credits`_ section.




.. _build-pel.el:           ../build-pel.el
.. _install-pel.el:         ../install-pel.el
.. _pel.el:                 ../pel.el
.. _pel--options.el:        ../pel--options.el
.. _pel--base.el:           ../pel--base.el
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
  such as `build-pel.el`_.
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
         You do not need to build PEL for using it.
         Most people will simply want to install and use PEL.
         If you are interested on how I byte-compile all files and how I prepare
         PEL to be distributed via an Emacs Lisp archive, then read on.

**Note 2:**
        At this moment, for this early version of PEL, I did not submit PEL
        package into Emacs Lisp archives like MELPA_.  I will do this later,
        once I've had time to add support for several programming languages and
        that I have completed the customization.

To control command line build of the PEL distribution package, the byte
compilation of all PEL Emacs Lisp source files, I wrote a GNU Makefile_ script
and also the Emacs Lisp file build-pel.el_

To see what you can do with the Makefile, move to the directory where it is
located and issue the ``make help`` command which lists the available top-level
targets and their purpose.

**Current Limitations**:

#. The current Makefile_ and build-pel.el_ assume that the files are
   located in a specific location.
#. Overall, this makefile is also a bit verbose and could be cleaned up.

.. _Makefile:             ../Makefile


PDF Documentation
-----------------

The list of documentation files are currently published as
several tables in `PDF files`_.
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

I am open to suggestions. And can provide the Numbers file on request.



.. _PDF files: `PDF Document Tables`_


Limitations & Bugs
------------------

#. **Dual-stage autoloading limitation**.
   PEL dual-stage autoloading currently will not show the PEL commands that are
   part of currently not loaded PEL files when trying to execute them with the
   ``M-x`` key or when attempting tab completion.  The command will be visible
   via ``<f1> o`` and ``<f1> f`` (or their ``C-h`` equivalent) but you'll see
   that their doc-strings are not loaded.  However, the PEL key-binding for the
   command will work, that will load the corresponding PEL file and then the
   ``M-x`` command and tab completion will work from then on.  Also before
   loading the file, if `which-key`_ is activated the commands will all show
   regardless of their load status.


Planning and Future
-------------------

At this point there's no formal planning for this project.

I have mainly been writing this as a way for me to learn and remember Emacs as a
tool and Emacs Lisp as a programming language.  But I am planning to use it for
most of my upcoming work and will continue to document what I learn and what I
use when developping in various programming languages and doing various tasks.


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

PEL uses the following libraries distributed with GNU Emacs and several others:

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
centimacro_                   MELPA_ (see [1]_)
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
.. _which-key:                 https://elpa.gnu.org/packages/which-key.html
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

Also, note that several ideas came from various blogs and discussion on the web.
There are references the these pages inside the PDF tables in the reference
sections, or also inside this manual.  This includes the following (non
exhaustive list):

- Jason Blevins blog: `Integrating OS X and Emacs Dired`_ .


.. _Integrating OS X and Emacs Dired: https://jblevins.org/log/dired-open

.. [1] Currently using `my fork of centimacro`_ until a bug fix I submitted
       gets integrated.


.. _my fork of centimacro: https://github.com/pierre-rouleau/centimacro



.. -----------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

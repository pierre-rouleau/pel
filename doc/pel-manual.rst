==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================



.. contents::  **Table Of Contents**
.. sectnum::


-----------------------------------------------------------------------------

Overview
========

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
   programming tasks.


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
===================

PEL is not yet available on a distribution repository like MELPA_
(Milkypostman's Emacs Lisp Package Archive).
Will it be?  I don't know yet.  I wonder if the nature of PEL's
project corresponds to what is accepted on MELPA.
PEL synthesizes information from various sources and integrates
several packages.
I will provide a MELPA recipe later and will see if the project gets accepted.

In the mean time, the easiest way to install PEL on one or several computers
is to create your own local Elpa-compatible package archive, include PEL inside that
and use this repository to install PEL and its dependencies. The following
sections describe how to that.


Prepare Emacs Before Installing PEL
-----------------------------------

Before installing PEL it's best to make sure that you already have some
configuration inside your `Emacs initialization file`_ described in the
following sub-sections.  In this document I assume that you have all your Emacs
files inside the "~/.emacs.d" directory, that your Emacs initialization file is
the file "~/.emacs.d/init.el".

Create a local package archive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Updates to your Emacs Initialization file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unfortunately *some* Emacs Lisp code must be written to your
`Emacs initialization file`_, but that's mainly to setup how to download packages
that you might already have, and possibly 2 lines to require and initialize PEL.



Configure How to Download Packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL uses
ELPA_ (GNU Emacs Lisp Package Archive)
and MELPA_ (Milkypostman's Emacs Lisp Package Archive)
sites to download and install packages.

To activate their use, place the following code inside your Emacs init file
(ideally with the block shown above) if
it is not already present:

.. code:: elisp

          (when (>= emacs-major-version 24)
            (require 'package)
            (setq package-enable-at-startup nil)
            (if (version=  emacs-version "26.2")
                (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

            (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                                (not (gnutls-available-p))))
                   (proto (if no-ssl "http" "https")))
              (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

              (when (< emacs-major-version 24)
                ;; For important compatibility libraries like cl-lib
                (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

            (package-initialize))

Select the location of Emacs Persistent Customization Data
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, Emacs stores its persistent customization data inside your
`Emacs initialization file`_.
If you want to store it somewhere else, and I recommend you do,
you have to tell Emacs where to read and write
the customization data.
Something like the following code
, which places it inside the file ``~/.emacs-custom.el``:

.. code:: elisp

          (setq custom-file "~/.emacs-custom.el")
          (load custom-file)

**Note**
   If you work inside several projects and each project requires different
   Emacs settings, you could use several customization files and activate them
   for each project, reducing the load time further.
   That provides another degree of freedom, along with Emacs directory local
   and file local variables.

To Install PEL
--------------

I have not yet submitted PEL to MELPA_.  That's on my to-do list.
For now clone the PEL Git repository  and perform manual installation.


Create an Emacs utility directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Create a directory to hold Emacs Lisp files that will be in Emacs ``load-path``.
For example, to store your utilities inside "~/.emacs.d/libs",  where you can
store single file utilities as well as packages with complete directory trees
(like pel) write the
following code inside your Emacs initialization file:

.. code:: elisp

          (add-to-list 'load-path (expand-file-name "~/.emacs.d/libs/pel"))
          (add-to-list 'load-path (expand-file-name "~/.emacs.d/libs"))


Clone the project from the Github page
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Clone the `PEL's Github repo`_ into the root of your utility directory,
("~/.emacs.d/libs" in this example) by doing the following inside a command
shell:


.. code:: shell

          cd ~/.emacs.d
          mkdir libs
          cd libs
          git clone https://github.com/pierre-rouleau/pel.git

This will create the "~/.emacs.d/libs/pel" directory with all the files inside it.

.. _PEL's Github repo: https://github.com/pierre-rouleau/pel

Byte-compile PEL files
~~~~~~~~~~~~~~~~~~~~~~

Inside the shell,
change directory to the location where they are stored, "~/emacs.d/libs/pel" if you
stored them there.  The directory contains a Makefile_.

To get a description of how to use the makefile, type::

  make help

Byte-compile all Emacs Lisp files in the directory by executing::

  make pel


Run pel-init
~~~~~~~~~~~~

- Open Emacs.
- Load pel.el by typing: ``M-x load-library RET pel RET``
- Run PEL initialization by typing: ``M-x pel-init RET``

This command will initialize PEL using the PEL's default customization.  If
these packages are not already installed on your system
``pel-init`` will download the following packages from MELPA_ and ELPA_
and store them inside the "~/.emacs.d/elpa" directory:

- `use-package`_
- `bind-key`_
- `which-key`_

PEL initialization should complete with PEL printing the following message on
the echo area::

  PEL loaded, PEL keys binding in effect

At this point you can use PEL with IDO, windmove and winner, which are all part
of the default customization.  To see if the PEL binding and `which-key`_ work,
just type the **F11** key to see the list o available commands and further key prefixes.

Customize PEL
~~~~~~~~~~~~~

If you want to add more features, change the value of the ``pel-use-...``
variable that correspond to the feature you want to activate.  Activating some
of them will force their installation when you next run ``pel-init``.  But not
all, because some o the features require files that are not part of an ELPA-style
library archive .   For those, you will have to copy their files inside a
directory in Emacs ``load-path`` like "~/.emacs.d/libs".
See `PEL Use Variables`_ for the list of variables and those that you may have
to install yourself.  For the ones that are automatically installed from ELPA_
or MELPA_ just set the corresponding variable and run ``pel-init``.

You can repeat the operation several times.  You can also exit Emacs between
them.  Just reload ``pel.el`` to be able to execute ``pel-init``.  You can run
``pel-init`` several times per Emacs session if you need to.



Complete PEL's Configuration
----------------------------

Once PEL code is installed, there's a couple more steps to follow.
They are described in the following sub-sections.

To start PEL when Emacs Starts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want PEL to be available right after Emacs starts, write the following
inside your Emacs init file:

.. code:: elisp

          (require 'pel)
          (pel-init)

If you do not want PEL to start when Emacs start,
then you can keep the first line but don't call ``pel-init``.
To use PEL later simply execute the **pel-init** command by
typing::

       M-x pel-init


To identify the location of your Ispell local dictionary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the current version of PEL, when you want to select the spell check
program used by
Ispell or Flyspell and the location of your personal dictionary you need to
write Emacs Lisp code in your Emacs init file that calls the ``pel-spell-init``
function.

The following is an example. It selects the ``aspell`` program
and identifies the path for the personal dictionary.

.. code:: elisp

          (eval-after-load "ispell"
            '(when (fboundp 'pel-spell-init)
                 (pel-spell-init “aspell" "~/.emacs.d/.ispell")))

In future versions of PEL, this code may not be necessary.

More information on PEL support of spell checking is available
in the `PEL Spell Checking Support`_ section.

To override or change PEL key bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As of this release PEL key bindings and key prefixes are hard coded.
If you want to change a key binding that PEL uses, you can define your own
key bindings after the execution of ``pel-init``.  You can also change
PEL prefix keys.

The following code re-assign the F6 key to ``undo`` and uses the F7 key
to be used as what PEL normally uses for F6:

.. code:: elisp

          (global-set-key (kbd "<f6>") 'undo)
          (global-set-key (kbd ("<f7>") pel:f6)


Generic Optimizations
---------------------

The following sections describe optimizations you can use anywhere, with or
without PEL.

Tricks to Speed-up your Emacs init time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL itself loads quickly.  But you can improve your overall Emacs initialization
time further by enclosing the entire code of your init.el file inside:

.. code:: elisp

          (let ((file-name-handler-alist nil)
                (gc-cons-threshold most-positive-fixnum))

            ;; all your initialization code goes here

          )

What the above does is to disable special file association handling and garbage
collection while Emacs processes your initialization code.  This has nothing to
do with PEL though.


..
   -----------------------------------------------------------------------------

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

:PDF Docs: `Abbreviations`_.
:PEL Customization: ``pel-use-hippie-expand``.
:PEL Key Prefix: **pel:abbrev** : ``<f11> a``

PEL provides automatic activation of Hippie expansion when the
``pel-use-hippie-expand`` customize variable is set to **t**.  Otherwise
it defaults to Dabbrev_ expansion.
PEL also provides the **pel:abbrev** key map which provides access to some
abbreviation related commands.  PEL binds it to ``<f11> a``.

All code provided by PEL about
abbreviations
is located inside the file `pel.el`_.


.. _Dabbrev: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html#Dynamic-Abbrevs




PEL Auto-Completion Support
---------------------------

:PDF Docs: `Auto-completion`_.
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

PEL Bookmark Management Utilities
---------------------------------

:PDF Docs: `Bookmarks`_.
:PEL Customization: ``pel-use-bm``.
:PEL Key Prefix: **pel:bookmark** : ``<f11> '``

The file `pel-bookmark.el`_ does not contain much.  It only provides the
utility function `pel-bookmark-in-current-file-p`` which checks if a bookmark of
a given name is present in the currently edited file.  This is used in other
parts of PEL.

For supporting bookmarks PEL provides the following:

- PEL provides a set of key bindings under the
  **pel:bookmark** key prefix set to ``<f11> '`` by default.
- If the ``pel-use-bm`` customize variable is set to **t** PEL add bindings to
  the visible bookmark commands and binds the **F2** key to ``bm-next`` which
  moves point to the next visible bookmark. PEL sets it to support bookmarks in
  several files and moving across files.
- Also, the project provides the `Bookmarks`_ PDF table which lists several
  bookmark related functions from various sources and their key bindings.

PEL Comments Utilities
----------------------

:PDF Docs: `Comments`_, `Cut, Delete, Copy and Paste`_, `Narrowing`_.
:PEL Customization: *none*
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



PEL Closing and Suspending Table
--------------------------------

:PDF Docs: `Closing and Suspending`_
:PEL Customization: *none*
:PEL Key Prefix: *none*

PEL provides the `Closing and Suspending`_ PDF table listing the Emacs commands
to close and suspend.


PEL Counting Support
--------------------

:PDF Docs: `Counting`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:count** : ``<f11> c``

PEL provides the **pel:count** key prefix (``<f11> c``) to Emacs commands that
count text and display results in the echo area.



PEL CUA Mode Extension Utilities - *experimental*
-------------------------------------------------

:PDF Docs: *none*
:PEL Customization: *none*
:PEL Key Prefix: *none*

**Note:**
   🚧  This file is under early development.

I'd like to find ways to easily manage rectangles of text without having to
activate the CUA mode and
the file `pel-cua.el`_ holds some experimental and unfinished code for going in
that direction.  Some of the commands are bound to PEL keys and described in the
PDF tables. But this work is in very early stage.


PEL Cut, Delete, Kill, Copy, Paste and Yank Utilities
-----------------------------------------------------

:PDF Docs: `Cut, Delete, Copy and Paste`_, `Marking`_.
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


PEL Drawing Support
-------------------

:PDF Docs: `Drawing`_.
:PEL Customization: *none*
:PEL Key Prefix: - **pel:draw** : ``<f11> d``

PEL provides key bindings to enter the Emacs text drawing modes:

- ``<f11> d a``: toggle artist mode.
- ``<f11> d p``: enter picture-mode.

☝️  The picture-mode can be quite useful to edit tabular data as well as editing
tables for markup languages like reStructuredText or even for lining text
vertically in any other type of file; for example lining up text vertically.


PEL Enriched Text Support
-------------------------

:PDF Docs: `Enriched Text`_.
:PEL Customization: *none*
:PEL Key Prefix: - **pel:textmodes** : ``<f11> t m``

PEL `Enriched Text`_ PDF table shows the Emacs commands available for
enriched text.



PEL File Management Utilities
-----------------------------

:PDF Docs: `File Management`_, `Dired`_.
:PEL Customization: ``pel-use-ido-mode``.
:PEL Key Prefix: **pel:file** : ``<f11> f``

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

When the ``pel-use-ido-mode`` customize variable is set to **t** ``pel-init``
activates IDO-mode_ everywhere, enables flex matching and prevents prompt when
creating new buffers with ``C-x b``.

.. _IDO-mode: https://www.gnu.org/software/emacs/manual/html_node/ido/index.html



PEL Font Management Utilities
-----------------------------

:PDF Docs: `Faces and Fonts`_.
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

:PDF Docs: `Frames`_.
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

:PDF Docs: `Function Keys`_, `F11 Keys`_.
:PEL Customization: *none*
:PEL Key Prefix: *N/A*

PEL avoids remapping most standard GNU Emacs key bindings.
Instead PEL uses the following function keys:

- **F2**, bound to ``bm-next`` (from `visible bookmarks`_)
  to quickly move to next visible bookmark
  when the ``pel-use-bm`` customize variable is **t**.
- **F5**, bound to ``repeat``.
- **F6**, the ``pel:f6`` prefix, which provides quick access to some
  often used commands.
- **F11**, the ``pel:`` prefix , is the main prefix key for PEL, providing
  access to a large set of key bindings and second-level key prefixes.
-  **F12** is a mode-sensitive key prefix with quick access bindings for the
   current major mode.


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

PEL Grep Support
----------------

:PDF Docs: `Grep`_.
:PEL Customization: ``pel-use-ripgrep``
:PEL Key Prefix: **pel:grep** : ``<f11> g``

PEL provides the **pel:grep** (``<f11> g``) key map to regroup grep commands.
If the ``pel-use-ripgrep`` customize variable is **t** that includes access to
the ``rg`` command that uses the fast ripgrep_ executable.

.. _ripgrep: https://github.com/BurntSushi/ripgrep


PEL Help Support
----------------

:PDF Docs: `Help`_
:PEL Customization: ``pel-use-free-keys``, ``pel-use-which-key``.
:PEL Key Prefix: - **pel:help** : ``<f11> h``

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


PEL Highlight and Whitespace Management Support
-----------------------------------------------

:PDF Docs: `Highlight`_ , `Whitespaces`_.
:PEL Customization: ``pel-use-rainbow-delimiters``
:PEL Key Prefix: - **pel-highlight** : ``<f11> b h``
                 - **pel-whitespace** : ``f11> t w``
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


PEL Indentation Support Utilities
---------------------------------

:PDF Docs: `Indentation`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:tab** : ``<f11> <tab>``

The file `pel-indent.el`_ includes some very basic utilities for simple
indentation control, complementing what is available in Emacs.
The available commands are:

- ``pel-insert-c-indent`` inserts spaces to indent the current line.
- ``pel-unindent`` removes spaces to un-indent the current line.
- ``pel-indent-rigidly`` indents the current line or marked region, this command
  extends the Emacs indent-rigidly command.

The PEL support for indentation will evolve as support form various types of
files, programming languages and markup languages evolves.


PEL Input Method Control
------------------------

:PDF Docs: `Input Method`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:text** : ``<f11> t``

PEL rebinds the ``C-\`` prime key, normally bound to ``toggle-input-method``,
used to select another `Emacs input method`_, to ``pel-kill-from-beginning-of-line``.
PEL binds ``toggle-input-method`` to ``<f11> t i`` instead.  And to change the
alternate input method, it binds ``set-input-method`` to ``<f11> t I``.
To lists all input methods, PEL provides ``<f11> ? d i`` bound to ``list-input-methods``.


.. _Emacs input method: https://www.gnu.org/software/emacs/manual/html_node/emacs/Input-Methods.html#Input-Methods


PEL Keyboard Macro Utilities
----------------------------

:PDF Docs: `Keyboard Macros`_.
:PEL Customization: ``pel-kmacro-prompts``.
:PEL Key Prefix: *none*

The file `pel-kmacros.el`- implements ``pel-kmacro-start-macro-or-insert-counter``
used to replace the standard ``kmacro-start-macro-or-insert-counter`` to record
a keyboard macro.  If the customize variable ``pel-kmacro-prompts`` is set to
**t**, the PEL function checks if the macro is already defined and if it is,
prompts before allowing to replace the existing keyboard macro with a new one.
It just offer a little protection.  And this protection can be reset by
executing the second command: ``pel-forget-recorded-keyboard-macro``.  In some
case that level of protection might be annoying, to disable it completely and
restore the normal Emacs keyboard macro recording without any protective
prompting, just set the ``pel-kmacro-prompts`` to *nil*.


PEL Line Control Utilities
--------------------------

:PDF Docs: `Display Lines`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:linectrl** : ``<f11> l``

The file ``pel-line-control.el`_ contains:

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

:PDF Docs: `Marking`_.
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

PEL reStructuredText Support Utilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:PDF Docs: `reStructuredText mode`_.
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

**Section Adornment Support**

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
- level 10 is using the <f12> 0`` key.

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

The default style is selected by the ``pel-rst-adornment-style`` customize variable.
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
``pel-use-rst-mode`` customize variable is set to **t**.



.. _reStructuredText: https://en.wikipedia.org/wiki/ReStructuredText
.. _external hyperlink targets: https://docutils.sourceforge.io/docs/user/rst/quickref.html#hyperlink-targets



PEL Menu Index Utilities
------------------------

:PDF Docs: `Menus`_.
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

:PDF Docs: `Narrowing`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*


PEL provides the  `Narrowing`_ PDF table listing Emacs commands dealing with
the powerful concept of narrowing.


PEL Navigation Support
----------------------

:PDF Docs: `Navigation`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

The `pel-navigate`_ file provides a collection of navigation commands that
complement the standard Emacs navigation commands.

- ``pel-beginning-of-line`` is meant to replace ``beginning-of-line`` as it does
  the same and extends it: if point is already at the beginning of the line
  then it moves it to the first non-whitespace character.
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


PEL Package Management Documentation
------------------------------------

:PDF Docs: `Packages`_.
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

PEL Common Lisp Support
~~~~~~~~~~~~~~~~~~~~~~~

:PDF Docs: `Common Lisp`_.
:PEL Customization: ``pel-use-rst-mode``.
:PEL Key Prefix: - Globally: **pel:for-lisp** : ``<f11> SPC L``
                 - From a buffer in lisp-mode: ``<f12>``


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


PEL Emacs Lisp Support
~~~~~~~~~~~~~~~~~~~~~~

:PDF Docs: `Emacs Lisp`_, `ERT`_.
:PEL Customization: ``pel-use-rst-mode``.
:PEL Key Prefix: - Globally: **pel:for-elisp** : ``<f11> SPC l``
                 - From a buffer in elisp-mode: ``<f12>``


The file `pel-lisp.el`_ contains command utilities that help edit Emacs Lisp
code.  Some of them can also be used for other types of Lisp as well.

- ``pel-toggle-lisp-modes`` toggles between ``lisp-interaction-mode`` and
  ``emacs-lisp-mode``.
- ``pel-byte-compile-file-and-load`` byte compiles the file in the current
  buffer and then load it.
- ``pel-lint-elisp-file`` runs Emacs Lisp lint on the current file.


PEL Prompt Utilities
--------------------

:PDF Docs: *none*
:PEL Customization: *none*
:PEL Key Prefix: *none*

The file `pel-prompt.el`_ is a utility file and for now only contains one
function: ``pel-y-n-e-or-l-p`` which prompts and accepts various types of
responses.  It is used by the ``pel-find-file-at-point-in-window`` command.
It's a rather specialized prompting utility with a rather strange name...

PEL Register Management Utilities
---------------------------------

:PDF Docs: `Register`_
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

The `Register`_ PDF document provides more information.

PEL Scrolling
-------------

:PDF Docs: `Scrolling`_.
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
customize variable is set to **t**, PEL provide a key binding to toggle smooth
scrolling on and off.  See the `Scrolling`_ PDF table for more info.

.. _smooth scrolling package: https://melpa.org/#/smooth-scrolling


PEL Search and Replace Support Utilities
----------------------------------------

:PDF Docs: `Search and Replace`_.
:PEL Customization: *none*
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


PEL Emacs Shell Support
-----------------------

:PDF Docs: `Shells`_.
:PEL Customization: ``pel-use-erlang``.
:PEL Key Prefix: **pel:eXecute** : ``<f11> x``

PEL provides the **pel:eXecute** (``<f11> x``) key binding to provide access to
various types of shells from within Emacs as described in the `Shells`_ PDF
table.

PEL Sorting Support
-------------------

:PDF Docs: `Sorting`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:order** : ``<f11> o``


PEL provides the **pel:order** (``<f11> o``) key binding to provide access to
Emacs commands you can use to sort (*order*) text in various ways as described
in the `Sorting`_ PDF table.


PEL Speedbar Management
-----------------------

:PDF Docs: `Speedbar`_.
:PEL Customization: ``pel-use-speedbar``, ``pel-prefer-sr-speedbar-in-terminal``.
:PEL Key Prefix: **pel:speedbar** : ``<f11> S``

The file `pel-speedbar.el`_ manages the accessibility and use of Emacs speed-bars:
both Emacs native Speedbar and the `SR-Speedbar`_ external package.
When the ``pel-use-speedbar`` customize variable is set to **t** PEL provides
key bindings for activating the speed-bars and provide some management
facilities. As shown in the PDF `Speedbar`_ table, Plus
default key bindings for those use the ``<f11> S`` prefix.

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

:PDF Docs: `Spell Checking`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:spell** : ``<f11> $``

The file `pel-spell.el`_ contains
spell checking utilities that detect and display what spell check mode is
active, and initialization code that fixes a problem with Flyspell pop-up
menu when Emacs runs in terminal (TTY) mode.

One of the goal of this file is to avoid loading either Ispell or Flyspell
until they are actually required while providing a function that can
configure these utilities: ``pel-spell-init``.

To configure Ispell and Flyspell without forcing early loading of the Ispell
and Flyspell libraries you can write something like the following inside your
init file:

.. code:: elisp

   (eval-after-load "ispell"
      '(when (fboundp 'pel-spell-init)
         (pel-spell-init "aspell"
                         "~/.emacs.d/.ispell")))

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

  - aspell 0.60.6.1
  - Ispell 3.3.0.2
  - enchant-2.2.7
  - hunspell 1.7.0

  Earlier versions of these programs were not tested, YMMV.


PEL Text Alignment Support
--------------------------

:PDF Docs: `Align`_.
:PEL Customization: *none*
:PEL Key Prefix: - **pel:align** : ``<f11> t a``

PEL provides the **pel:align** key binding ``<f11> t a`` to Emacs text alignment
commands.



PEL Text Filling and Justification Utilities
-------------------------------------------

:PDF Docs: `Filling and Justification`_, `Text-modes`_.
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

:PDF Docs: `Inserting Text`_.
:PEL Customization: ``pel-use-lice``.
:PEL Key Prefix: **pel:insert** : ``<f11> i``

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

The PEL binding include more commands, some are Emacs standard commands, other
are from other packages.  All are listed in the `Inserting Text`_ PDF
documentation.


PEL Text Transformation Utilities
---------------------------------

:PDF Docs: `Case Conversion`_, `Text-modes`_.
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

:PDF Docs: `Transpose`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:text-transpose** : ``<f11> t t``


PEL provides the **pel:text-transpose** key prefix (``<f11 t t``) to a set of Emacs
commands that transpose text, as shown in the `Transpose`_ PDF table.


PEL Undo Support
----------------

:PDF Docs: `Undo, Redo, Repeat and Prefix Arguments`_.
:PEL Customization: ``pel-use-undo-tree``, ``pel-use-goto-last-change``.
:PEL Key Prefix: **pel:undo** : ``<f11> u``

PEL provides the **pel:undo** key prefix (``<f11> u``) to Emacs undo commands.
If the ``pel-use-undo-tree`` customization variable is set to **t**, it uses the
undo-tree package to control undo and binds its keys.
If the ``pel-use-goto-last-change`` customization variable is set to **t** it
also provides access to the ``goto-last-change`` command and binds it.
All key binding details are in the `Undo, Redo, Repeat and Prefix Arguments`_ PDF table.


PEL Window Management Support
-----------------------------

:PDF Docs: `Windows`_.
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
It currently uses the **F2**, **F6**, **F11** and **F12** keys as prefix keys.
It also binds **F5** as the repeat key.
In this version these bindings are hard-coded.

**Note:**
         If you have already bound these keys to something else, you can easily
         override PEL's binding by placing your own binding statements in your
         Emacs init file **after** the call to ``pel-init``.
         You can also use
         PEL's key-maps but change the prefix keys.
         See the section titled `To override or change PEL key bindings`_ for
         more info.


The best way to quickly see the list of PEL prefix key is right inside Emacs.
Type the prefix key (like **F11**) and then quickly type
either **C-h** or **F1**.
Emacs will open a ``*help*`` buffer that lists all keys available.  You can
navigate this buffer and follow the links to the described commands. To get the
list of the keys for a sub-prefix type it and again follow with
either **C-h** or **F1**.

The following table lists the **F11** key map as an example.
As described in the `Naming Conventions`_ section the names in the binding
column that use the "pel:" prefix are sub key-maps.
The commands use the prefix "pel-".
As you can see some of the commands are accessible right after the **F11**
prefix, but there's a large number of sub-prefix following.
The key-map names were chosen to be as descriptive as possible and use keys that
mnemonically associate to the related concept if at all possible.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f11> SPC``                   Prefix Command to access the
                                `PEL Mode Sensitive Key-maps`_
                                from any buffer.
``<f11> TAB``                   pel:indent
``<f11> #``                     **pel-toggle-mac-numlock**
``<f11> $``                     pel:spell
``<f11> '``                     pel:bookMark
``<f11> +``                     **pel-copy-marked-or-whole-line**
``<f11> ,``                     pel:auto-completion
``<f11> -``                     pel:kill
``<f11> .``                     pel:mark
``<f11> 0``                     **hl-line-mode**
``<f11> ;``                     pel:comment
``<f11> =``                     pel:copy
``<f11> ?``                     pel:help
``<f11> C``                     pel:clipboard
``<f11> F``                     pel:frame
``<f11> S``                     pel:speedbar
``<f11> [``                     **pel-cua-move-rectangle-left**
``<f11> ]``                     **pel-cua-move-rectangle-right**
``<f11> a``                     pel:abbrev
``<f11> b``                     pel:buffer
``<f11> c``                     pel:count
``<f11> d``                     pel:draw
``<f11> f``                     pel:file
``<f11> g``                     pel:grep
``<f11> i``                     pel:insert
``<f11> k``                     pel:kbmacro
``<f11> l``                     pel:linectrl
``<f11> o``                     pel:order
``<f11> r``                     pel:register
``<f11> s``                     pel:search-replace
``<f11> t``                     pel:text
``<f11> u``                     pel:undo
``<f11> w``                     pel:window
``<f11> x``                     pel:eXecute
``<f11> |``                     pel:scroll
``<f11> <f10>``                 pel:menu
``<f11> <f11>``                 **pel-toggle-frame-fullscreen**
``<f11> <f12>``                 **xterm-mouse-mode**
``<f11> <C-S-down>``            **pel-close-window-down**
``<f11> <C-S-left>``            **pel-close-window-left**
``<f11> <C-S-right>``           **pel-close-window-right**
``<f11> <C-S-up>``              **pel-close-window-up**
``<f11> <C-down>``              **pel-create-window-down**
``<f11> <C-left>``              **pel-create-window-left**
``<f11> <C-right>``             **pel-create-window-right**
``<f11> <C-up>``                **pel-create-window-up**
``<f11> <M-left>``              **pel-backward-syntaxchange-start**
``<f11> <M-right>``             **pel-forward-syntaxchange-start**
``<f11> <C-f10>``               **menu-bar-mode**
``<f11> <down>``                **windmove-down**
``<f11> <left>``                **windmove-left**
``<f11> <right>``               **windmove-right**
``<f11> <up>``                  **windmove-up**
=============================== ===========================================

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
the **F12** key has the following bindings.

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

PEL comes with a set of tables listing and describing both the
**standard GNU Emacs**
commands and key bindings for a given type of activity along with the extra
commands provided by PEL.
These tables are inside PDF documents.
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

Note:
  The hyperlinks are not active when viewed through Github rendering.
  They are active when rendered directly by a browser or a PDF viewer.

PDF Document Tables
~~~~~~~~~~~~~~~~~~~

- `Document Legend`_

**Emacs Concepts**

#. `File and Directory Local Variables`_
#. `Modifier Keys`_

**Emacs Operations:**

#. `Abbreviations`_
#. `Align`_
#. `Auto-Completion`_
#. `Bookmarks`_
#. `Buffers`_
#. `Case Conversion`_
#. `Closing and Suspending`_
#. `Comments`_
#. `Counting`_
#. `Cut, Delete, Copy and Paste`_ (killing and yanking)
#. `Display Lines`_
#. `Enriched Text`_
#. `Faces and Fonts`_
#. `File Management`_
#. `Filling and Justification`_
#. `Frames`_
#. `Grep`_
#. `Help`_
#. `Highlight`_
#. `Hooks`_
#. `Indentation`_
#. `Input Method`_
#. `Inserting Text`_
#. `Keyboard Macros`_
#. `Marking`_
#. `Menus`_
#. `Narrowing`_
#. `Navigation`_
#. `Packages`_
#. `Registers`_
#. `Scrolling`_
#. `Search and Replace`_
#. `Shells`_
#. `Sorting`_
#. `Speedbar`_
#. `Spell Checking`_
#. `Text-modes`_
#. `Transpose`_
#. `Undo, Redo, Repeat and Prefix Arguments`_
#. `Web`_
#. `Whitespaces`_
#. `Windows`_

**Modes:**

#. `Dired`_
#. `Graphviz Dot`_
#. `Org mode`_
#. `reStructuredText mode`_

**Programming Language Support:**

#. `Common Lisp`_
#. `Emacs Lisp`_

   - `ERT`_ (Emacs Lisp Regression Testing system)

**Version Control Systems:**

#. `Mercurial`_

**Miscellaneous**

#. `Function Keys`_
#. `F11 Keys`_
#. `macOS Terminal settings`_

.. _Document Legend:                          pdf/-legend.pdf
.. _Abbreviations:                            pdf/abbreviations.pdf
.. _Align:                                    pdf/align.pdf
.. _Auto-Completion:                          pdf/auto-completion.pdf
.. _Bookmarks:                                pdf/bookmarks.pdf
.. _Buffers:                                  pdf/buffers.pdf
.. _Case Conversion:                          pdf/case-conversion.pdf
.. _Closing and Suspending:                   pdf/closing-suspending.pdf
.. _Comments:                                 pdf/comments.pdf
.. _Counting:                                 pdf/counting.pdf
.. _Cut, Delete, Copy and Paste:              pdf/cut-paste.pdf
.. _Display Lines:                            pdf/display-lines.pdf
.. _Drawing:                                  pdf/drawing.pdf
.. _Enriched Text:                            pdf/enriched-text.pdf
.. _ERT:                                      pdf/ert.pdf
.. _Faces and Fonts:                          pdf/faces-fonts.pdf
.. _File Management:                          pdf/file-mngt.pdf
.. _File and Directory Local Variables:       pdf/file-variables.pdf
.. _Filling and Justification:                pdf/filling-justification.pdf
.. _Frames:                                   pdf/frames.pdf
.. _Function Keys:                            pdf/keys-fn.pdf
.. _F11 Keys:                                 pdf/keys-f11.pdf
.. _Graphviz Dot:                             pdf/graphviz-dot.pdf
.. _Grep:                                     pdf/grep.pdf
.. _Help:                                     pdf/help.pdf
.. _Highlight:                                pdf/highlight.pdf
.. _Hooks:                                    pdf/hooks.pdf
.. _Indentation:                              pdf/indentation.pdf
.. _Input Method:                             pdf/input-method.pdf
.. _Inserting Text:                           pdf/inserting-text.pdf
.. _Keyboard Macros:                          pdf/keyboard-macros.pdf
.. _Marking:                                  pdf/marking.pdf
.. _Menus:                                    pdf/menus.pdf
.. _Dired:                                    pdf/mode-dired.pdf
.. _Org mode:                                 pdf/mode-org-mode.pdf
.. _reStructuredText mode:                    pdf/mode-rst.pdf
.. _Modifier Keys:                            pdf/modifier-keys.pdf
.. _Narrowing:                                pdf/narrowing.pdf
.. _Navigation:                               pdf/navigation.pdf
.. _Packages:                                 pdf/packages.pdf
.. _Common Lisp:                              pdf/pl-common-lisp.pdf
.. _Emacs Lisp:                               pdf/pl-emacs-lisp.pdf
.. _Registers:                                pdf/registers.pdf
.. _Scrolling:                                pdf/scrolling.pdf
.. _Search and Replace:                       pdf/search-replace.pdf
.. _Shells:                                   pdf/shells.pdf
.. _Sorting:                                  pdf/sorting.pdf
.. _Speedbar:                                 pdf/speedbar.pdf
.. _Spell Checking:                           pdf/spell-checking.pdf
.. _Text-modes:                               pdf/text-modes.pdf
.. _Transpose:                                pdf/transpose.pdf
.. _Undo, Redo, Repeat and Prefix Arguments:  pdf/undo-redo-repeat.pdf
.. _Mercurial:                                pdf/vsc-mercurial.pdf
.. _Web:                                      pdf/web.pdf
.. _Whitespaces:                              pdf/whitespaces.pdf
.. _Windows:                                  pdf/windows.pdf
.. _macOS Terminal settings:                  pdf/macOS-terminal-settings.pdf



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
       **F11** and **F12** keys.
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








PEL Use Variables
-----------------

The following table contains the list of the ``pel-use-`` customize variables
currently available.

**Note**:

- Several of the options listed in the table below identify that PEL attempts to
  install the package if it is not present. PEL implements this using the code
  generated by the `use-package`_ *package*.  If you prefer installing the
  packages yourself, install them **before** setting the corresponding
  ``pel-use-`` variable and running the ``pel-init`` command.
  Running ``pel-init`` while the variable is ``nil`` will not force installation.

============================== ============================================================= =================
Variable                       Purpose and link to more info                                 Attempts Install
============================== ============================================================= =================
pel-use-ace-window             Enables use of the `ace-window package`_ to                   Yes, from MELPA_.
                               be able to navigate across windows easily.

                               ☝️  See `PEL Window Management Support`_.

pel-use-auto-complete          Enables use of the `auto-complete package`_                   Yes, from MELPA_.
                               which provides auto-completion while typing.

                               **Note**: 🚧 only basic support is provided.
                               Better integration for various programming
                               language is planned.

                               ☝️  See `PEL Auto-Completion Support`_.


pel-use-bind-key               Enables use of the `bind-key`_ package for some               No, it comes
                               PEL commands that use it.                                     with use-package
                                                                                             required by PEL.
                               - This package is not distributed with Emacs.
                               - It is, however installed when you install PEL because
                                 PEL depends on `use-package`_ which depends on `bind-key`_.

pel-use-bm                     Enables use of the bm_ package, which provides                Yes, from MELPA_.
                               visible bookmarks.  When enabled, PEL provides some key
                               bindings for it.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                               - If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.

                               ☝️  See `PEL Bookmark Management Utilities`_.

pel-use-c-eldoc                Enables use of the `c-eldoc`_ package which                   Yes, from MELPA_.
                               provides helpful descriptions of the arguments to C functions
                               when editing a buffer in c-mode.  PEL sets the hook required
                               for this.

pel-use-cc-vars                Enables use of the cc-vars standard Emacs                     No, it is part
                               library for the cc mode.  PEL sets some values for C          of standard GNU
                               development.                                                  Emacs.

                               **Note**: 🚧 support for this is underway.
                               More options to be documented once C development is described
                               in the PEL documentation.

pel-use-common-lisp            Enables use of Common Lisp development within                 Yes, it tries to
                               Emacs using a Common Lisp system such as SBCL_  (Steel Bank   install slime
                               Common Lisp).                                                 from your site
                                                                                             preference.
                               When activated PEL attempts to install the `slime package`_.  It does not
                                                                                             install
                                                                                             Common Lisp.

                               **Note**: 🚧 Common Lisp support is not completed.
                               Several aspects need customization, like for example,
                               the location of the Hyperspec.  Better support for
                               Common Lisp is planned.


pel-use-company                Enables the use of the company_ package, one                  Yes, from MELPA_.
                               of the PEL supported Emacs packages for auto-completion.

                               **Note**: 🚧 only basic support is provided.
                               Better integration for various programming
                               language is planned.

                               ☝️  See `PEL Auto-Completion Support`_.


pel-use-dired-narrow           Enables the use of the dired-narrow_                          Yes, from MELPA_.
                               package.  This package provides commands to quickly
                               reduce the number of entries shown in the ``*dired*``
                               buffer.

pel-use-edts                   *Future*. Reserved to control the use of the                  Not for this
                               Erlang Development Tool Suite.                                version.

pel-use-eglot                  *Future*.  Reserved to control the use of the                 Not for this
                               eglot Language Server Protocol.                               version.
                               This will be introduced with support for programming
                               languages that use it

pel-use-eldoc-box              *Future*.  Reserved to control the use of the                 Not for this
                               eldoc-box package which displays Eldoc information            version.
                               inside child frame.

pel-use-erlang                 *Future*.  Reserved to control use of Erlang                  Not for this
                               support.                                                      version.

pel-use-erlang-flymake         *Future*.  Reserved to control use of Erlang                  Not for this
                               support.                                                      version.

pel-use-erlang-start           *Future*.  Reserved to control use of Erlang                  Not for this
                               support.                                                      version.

pel-use-esup                   Enables the use of the esup_                                  Yes, from MELPA_.
                               package, the Emacs StartUp Profiler.

pel-use-expand-region          Enables the use of the                                        Yes, from MELPA_.
                               expand-region_ package which provides a powerful
                               text selection mechanism.

                               ☝️  See `PEL Mark Management Utilities`_.


pel-use-framemove              Activates and enables the use of the                          Not yet.
                               framemove_ package. This extends the windmove                 This is hosted
                               feature allowing to quickly move point to another             in the EmacsWiki
                               frame using the same mechanism as with windmove.              not in any ELPA
                                                                                             archive.
                               For now, this must be installed manually from
                               the framemove_ site.

                               ☝️  See `PEL Frame Management Utilities`_.


pel-use-free-keys              Enables the use of of the free-keys_ package                  Yes, from MELPA_.
                               that lists the unused key-bindings.

pel-use-goto-last-change       Enables the use of the goto-last-change_                      Yes, from MELPA_.
                               package.

pel-use-graphviz-dot           Enables the use of the graphviz-dot-mode_                     Yes, from MELPA_.
                               package to edit Graphviz DOT files and generate
                               graphs from within Emacs.

pel-use-highlight-defined      Enables the use of the highlight-defined_                     Yes, from MELPA_.
                               package which highlights defined Emacs Lisp
                               symbols that are defined.

pel-use-hippie-expand          Activates the use of the Emacs built-in                       Nothing to
                               hippie-expand for abbreviation expansion instead of the       install. It's
                               default dabbrev-expand.                                       part of Emacs.

pel-use-ido-mode               Activates the use of Emacs built-in ``IDO-mode``.             Nothing to
                                                                                             install. It's
                               ☝️  See `PEL File Management Utilities`_.                      part of Emacs.

pel-use-lice                   Enables the use of the lice_ package to add open              Yes, from MELPA_.
                               source license text in buffers.

                               ☝️  See `PEL Text Insertion Utilities`_.

pel-use-macrostep              Enables the use of the macrostep_ package to                  Yes, from MELPA_.
                               expand Lisp macros right inside Emacs.

pel-use-nhexl-mode             Enables the use of the nhexl-mode_ to edit                    Yes, from ELPA_.
                               binary files.

pel-use-org-mode               When set, configures Org-Mode and provide key                 Nothing to
                               binding similar to a subset of what is suggested              install. It's
                               in the Org Mode manual.                                       part of Emacs.
                               This is a *tinkering experiment* and is likely
                               to change in the future.  It's far from having
                               providing a decent additional feature to
                               Org-Mode.

pel-use-parinfer               Enables the use of the parinfer_ package that                 Yes, from MELPA_.
                               infers Lisp parenthesis location.

pel-use-popup-kill-ring        Enables the use of the popup-kill-ring_                       Yes, from MELPA_.
                               package that provides ability to see the
                               content of the kill ring in a pop-up menu,
                               filter by text and see what to select and
                               insert.

pel-use-python                 Enables use of basic Python support.                          Nothing to
                               Future versions of PEL will include                           install. It just
                               more Python support facilities enabled                        uses what's part
                               with this.                                                    of Emacs.

pel-use-rainbow-delimiters     Enables the use of rainbow-delimiters_ package                Yes, from MELPA_.
                               to colorize matching *parens*.

pel-use-re-builder             Enables the loading of the re-builder built-in                Nothing to
                               library for regular expression builder.                       install.  It's
                                                                                             part of Emacs.

pel-use-ripgrep                Enables the use of the rg_ package which uses                 Yes, from MELPA_.
                               ripgrep command line utility to perform                       However, it does
                               fast grep operations.                                         install ripgrep.

pel-use-rst-mode               Enables the use of the rst built-in library                   Nothing to
                               to support reStructuredText.                                  install.  It's
                               The current PEL version sets the                              part of Emacs.
                               section adornments.  Future versions will
                               be modified to help using rst customization
                               and will add more features.

                               ☝️  See `PEL reStructuredText Support Utilities`_.

pel-use-rust                   Enables support for Rust programming language                 Yes, all from
                               via rust-mode_, racer_ and cargo_.                            MELPA_.
                               If pel-use-company is also set to **t** it activates
                               company-mode to be used with racer.

pel-use-speedbar               Enables support of the sr-speedbar_ package to provide        Yes, from MELPA_.
                               Speedbar inside the same frame, useful in terminal (TTY)
                               mode.

                               ☝️  See `PEL Speedbar Management`_.

pel-use-undo-tree              Enables support of the undo-tree_ package to help manage      Yes, with ELPA_.
                               the undo buffer.

pel-use-uniquify               Activates the use of the uniquify library (distributed        Nothing to
                               with standard GNU Emacs) which helps give a unique name to    install.  It's
                               buffers visiting files or directories with the same names.    part of Emacs.

pel-use-which-key              Enables the use of the which-key_ package.                    Yes, from ELPA_.
                               This is recommended for new users, as it shows the keys
                               available after each prefix key.
                               PEL key binding system was designed to show nicely when
                               which-key is used. It default to nil.

pel-use-winner                 Enables the use and the PEL key-bindings of winner-mode.      Nothing to
                                                                                             install. The
                                                                                             winner package
                                                                                             is part of Emacs.
============================== ============================================================= =================



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
   - This file cannot be byte-compiled alone by an ``emacs -Q`` session
     without warnings because it uses `use-package`_ logic to control
     the installation and use of external packages that are most likely
     not present on a new installation.  It also has logic to use other
     packages that are not available from MELPA_ or ELPA_.
     All of these packages are only used conditionally when a ``pel-use-``
     customize variable corresponding to the needed package activates its
     use. That allows the code to run without any issue when the activated
     package is present and when absent packages are not activated.
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

Currently, PEL only uses `use-package`_
to control the installation of missing package if the corresponding feature
is activated via `PEL customization`_ ``pel-use-`` customization variable.
However, when using `Emacs package-install`_ to install PEL, then all
dependencies identified by the `pel-pkg.el`_ file will be installed as well.
They will be located inside your Emacs load-path but will only be loaded if the
corresponding ``pel-use-`` customize variable is set to **t**.

Note that this mechanism only works for external packages that are available from an
Elpa compatible Emacs package archive site (ELPA_, MELPA_, a local Elpa archive,
etc...)
Some of the packages PEL uses are not hosted on these sites (yet) but on site
like EmacsWiki_.  For the moment those packages must be installed manually.
The list of external packages used by PEL is shown in the `Credits`_ section.




.. _build-pel.el:           ../build-pel.el
.. _install-pel.el:         ../install-pel.el
.. _pel.el:                 ../pel.el
.. _pel--options.el:        ../pel--options.el
.. _pel--base.el:           ../pel--base.el
.. _pel--macros.el:         ../pel--macros.el
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
.. _pel-cua:
.. _pel-cua.el:             ../pel-cua.el
.. _pel-file:
.. _pel-file.el:            ../pel-file.el
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
.. _pel-register:
.. _pel-register.el:        ../pel-register.el
.. _pel-rst:
.. _pel-rst.el:             ../pel-rst.el
.. _pel-scroll:
.. _pel-scroll.el:          ../pel-scroll.el
.. _pel-search:
.. _pel-search.el:          ../pel-search.el
.. _pel-speedbar:
.. _pel-speedbar.el:        ../pel-speedbar.el
.. _pel-spell:
.. _pel-spell.el:           ../pel-spell.el
.. _pel-text-insert:
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
    names; the `pel--base.el`_ and `pel--options`_ files are used by the other
    PEL files.

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

The Emacs Lisp code is written to comply with the `standard GNU Emacs code
guidelines`_.  PEL code follows most of the points promoted by
`bbatsov/emacs-lisp-style-guide`_ except in some few places.
PEL code also follows several ideas outlined in
`Jari Aalto Emacs Lisp Guideline`_, an older but still valid guideline.

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

Then I create a local Elpa Emacs Archive and run the following command to build
PEL archive and place it inside the local archive:

.. code:: shell

          make myelpa

Once this is done, I use the switch-emacs script (`see below`_) to switch
the ``.emacs.d`` directory to
something with minimal configuration, (normally called "~/min-emacs")
with enough to set the ``load-path`` and
access to ELPA_, MELPA_ and MELPA-STABLE_ and the local archive where ``pel``
was stored.

I also wipe out the directory "~/.emacs.d/elpa" directory to remove all the
packages from it.

Using this minimal Emacs configuration I then execute Emacs.  That instance of
Emacs does not have access to PEL (yet).  So then I use ``M-x list-packages``
and install ``pel`` from that, using ``package-install``.
The ``pel`` package comes from the local archive
but everything else comes from the real sites.
The ``package-install`` command downloads all PEL pre-requisites and byte-compile
PEL files.  I make sure no warning is issued from the PEL files.

.. _see below:                         `The switch-emacs script`_


The switch-emacs script
~~~~~~~~~~~~~~~~~~~~~~~

The following ``switch-emacs`` script renames "~/.emacs.d" to either
"~/real-emacs" or "~/min-emacs" depending which one exists.
The "~/real-emacs" is a temporary name for the *real* Emacs configuration directory,
while "~/min-emacs" is a minimal Emacs configuration to allow testing Emacs with
a minimal configuration. To use the script you must first create "~/min-emacs"
and a ~/min-emacs/init.el" file that provides enough code to set the ``load-path``
and access to the external and local Emcas archive repositiories.

.. code:: shell

          #!/bin/sh
          # Exchange ~/.emacs.d -> ~/real-emacs
          #          ~/min-emacs -> ~/.emacs.d
          # or vice versa: exchange real emacs for a new emacs directory.
          #
          # ~/min-emacs  : a minimal Emacs configuration to perform package-install PEL tests.
          # ~/real-emacs : a temporary storage for the *real* emacs configuration directory.

          cd
          if [[ -d "min-emacs" ]]; then
              echo "---> min-emacs exists. Activating the min-emacs."
              mv .emacs.d real-emacs
              mv min-emacs .emacs.d

          elif [[ -d "real-emacs" ]]; then
              echo "---> real-emacs exists.  Activating the real-emacs."
              mv .emacs.d min-emacs
              mv real-emacs .emacs.d
          else
              echo "First create a ~/min-emacs directory with minimal init.el"
              echo "The ~/min-emacs/init.el should:"
              echo " - set load-path to find utilities not compig from elpa-compatible archive,"
              echo " - set package-archives list to include melpa, melpa-stable, gnu elpa,"
              echo "   and your local myelpa."
              echo "It should not load PEL nor execute pel-init."
          fi


.. _elint.el:                          https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/elint.el
.. _standard GNU Emacs code guidelines:
.. _Emacs Lisp checkdoc:               https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html
.. _bbatsov/emacs-lisp-style-guide:    https://github.com/bbatsov/emacs-lisp-style-guide
.. _Jari Aalto Emacs Lisp Guideline:   http://www.nongnu.org/emacs-tiny-tools/elisp-coding/index-body.html
.. _Some Performance Advantages of Lexical Scope: https://nullprogram.com/blog/2016/12/22/

Emacs Lisp Regression Test
--------------------------

At this point just a small portion of PEL is covered by
`ERT based <https://www.gnu.org/software/emacs/manual/html_node/ert/index.html>`_
testing.  The test code is located inside the `test sub-directory`_.
As everything in PEL for this early version: 🚧 more to come here...

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
#. The ``make``, ``make all``  always rebuild everything regardless of
   the state and dependencies of the files.
#. Overall, this makefile is also a bit verbose and could be cleaned up.

These defects currently don't prevent me from using the
Makefile but do bug me, so that's another thing on my 🚧 todo list.


.. _Makefile:             Makefile


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


..
   -----------------------------------------------------------------------------

Security Aspects
================

🚧 To be written...


Credits
=======

PEL integrates with several great Emacs Lisp packages.  Some of them are
required, the others are used only if they are present and are activated by the PEL
customization.  PEL's code is written to operate despite the absence of external
packages that have not been activated but it expects presence of packages that are
distributed with GNU Emacs.

PEL uses the following libraries distributed with GNU Emacs:

- bookmark
- cc-vars
- cua-rect
- delsel
- elint
- ert
- flyspell
- hippie-exp
- hl-line
- ido
- imenu
- isearch
- ispell
- kmacro
- lisp-mode
- newcomment
- org
- paragraphs
- re-builder
- rst
- simple
- subr-x
- subword
- thingatpt
- uniquify
- winner

PEL has the following dependencies on the following external Emacs packages:

============================= ==========================
Package Name & Archive link   Archive Site
============================= ==========================
`ace-window`_                 MELPA_
`auto-complete`_              MELPA_
`bind-key`_                   MELPA_
`bm`_                         MELPA_
`c-eldoc`_                    MELPA_
`cargo`_                      MELPA_
`company`_                    MELPA_
`dired-narrow`_               MELPA_
`edts`_                       MELPA_
elpy_                         MELPA_
esup_                         MELPA_
expand-region_                MELPA_
framemove_                    EmacsWiki_
free-keys_                    MELPA_
goto-last-change_             MELPA_
graphviz-dot-mode_            MELPA_
highlight-defined_            MELPA_
popup-kill-ring_              MELPA_
lice_                         MELPA_
macrostep_                    MELPA_
nhexl-mode_                   ELPA_
parinfer_                     MELPA_
popup_                        MELPA-STABLE_
popup-kill-ring_              MELPA_
racer_                        MELPA_
rainbow-delimiters_           MELPA_
rg_                           MELPA_
rust-mode_                    MELPA_
slime_                        MELPA_
smooth-scrolling_             MELPA_
sr-speedbar_                  MELPA_
undo-tree_                    ELPA_
`use-package`_                MELPA_
which-key_                    MELPA_
============================= ==========================


For developing PEL, the following extra packages are used.

============================= ==========================
Package Name & Archive link   Archive Site
============================= ==========================
elisp-lint_                   MELPA_
package-lint_                 MELPA_
============================= ==========================



.. References

.. _ace-window:
.. _ace-window package:        https://melpa.org/#/ace-window
.. _auto-complete:
.. _Auto Complete:
.. _auto-complete package:     https://melpa.org/#/auto-complete
.. _MELPA:                     https://melpa.org/
.. _MELPA-STABLE:              https://stable.melpa.org/
.. _use-package:               https://melpa.org/#/use-package
.. _bind-key:                  https://melpa.org/#/bind-key
.. _visible bookmarks:
.. _bm:                        https://melpa.org/#/bm
.. _cargo:                     https://melpa.org/#/cargo
.. _company:                   https://melpa.org/#/company
.. _c-eldoc:                   https://melpa.org/#/?q=c-eldoc
.. _dired-narrow:              https://melpa.org/#/dired-narrow
.. _edts:                      https://melpa.org/#/edts
.. _elisp-lint:                https://melpa.org/#/elisp-lint
.. _elpy:                      https://melpa.org/#/elpy
.. _esup:                      https://melpa.org/#/esup
.. _expand-region:             https://melpa.org/#/expand-region
.. _free-keys:                 https://melpa.org/#/free-keys
.. _goto-last-change:          https://melpa.org/#/goto-last-change
.. _graphviz-dot-mode:         https://melpa.org/#/graphviz-dot-mode
.. _highlight-defined:         https://melpa.org/#/highlight-defined
.. _lice:                      https://melpa.org/#/lice
.. _macrostep:                 https://melpa.org/#/macrostep
.. _nhexl-mode:                https://elpa.gnu.org/packages/nhexl-mode.html
.. _package-lint:              https://melpa.org/#/package-lint
.. _parinfer:                  https://melpa.org/#/parinfer
.. _popup:                     https://stable.melpa.org/#/popup
.. _popup-kill-ring:           https://melpa.org/#/popup-kill-ring
.. _racer:                     https://melpa.org/#/racer
.. _rainbow-delimiters:        https://melpa.org/#/rainbow-delimiters
.. _rg:                        https://melpa.org/#/rg
.. _rust-mode:                 https://melpa.org/#/rust-mode
.. _slime:                     https://melpa.org/#/slime
.. _slime package:             https://melpa.org/#/slime
.. _smooth-scrolling:          https://melpa.org/#/smooth-scrolling
.. _sr-speedbar:               https://melpa.org/#/sr-speedbar
.. _undo-tree:                 https://elpa.gnu.org/packages/undo-tree.html
.. _which-key:                 https://elpa.gnu.org/packages/which-key.html
.. _SBCL:                      https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
.. _ELPA:                      https://elpa.gnu.org
.. _framemove:                 https://www.emacswiki.org/emacs/FrameMove

-----------------------------------------------------------------------------

..
   -----------------------------------------------------------------------------

..  LocalWords:  PEL

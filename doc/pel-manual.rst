==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================



.. contents::  **Table Of Contents**
.. sectnum::


Overview
========

PEL is a package that gets you going with Emacs quickly while allowing
you to continue using your current Emacs configuration.
It's a compromize between a full blown starter kit or Emacs environment
and a bare-bone Emacs.
It keeps most of the Emacs key bindings untouched while providing
quick access to several other packages,  extended features
and key binding trees.
Most features are activated via the Emacs customization
system, providing control without requiring extra Emacs Lisp code
(except for 2 forms to require and init PEL).

**Note**:
   This is an early version of PEL, and my first contribution to Emacs.
   It will grow with time, incorporating
   more Emacs packages to support more editing tasks and
   programming languages.


The PEL package provides:

- Pre-canned configuration of several useful Emacs packages
  that loads quickly and which is controlled by Emacs customization
  system.
  This help getting going with Emacs without having to know
  the art of writing Emacs init.el file.
- The configuration is made to load quickly, making extensive
  use of auto-loading and deferred loading is used everywhere.
- The selection of package and features is done via PEL customization
  variables.
- Dynamic control of several packages and their commands.
  For example, PEL support both auto-complete and company auto-completion
  pakages, providing commands to activate one mode in a buffer and
  another mode inside another and while preventing dual use in a buffer.
- Key bindings using function key prefixes (**F2**, **F6**, **F11** and **F12**)
  to provide easy access to many features and help learn what's available.

  - Most standard Emacs keys are left untouched.
  - The `which-key`_ package is used and activated by default, allowing
    you to see what's available easily.  **F11** is the main prefix key
    and all prefixes have a meaningful name that starts with the
    'pel:' prefix.  **F2** and **F6** are used as global shortcut prefix keys,
    and **F12** as a mode-sensitive shortcut prefix key.
  - See the `Key Binding Documentation`_ section for more info.

- PEL comes with a set of convenience features that deal with several
  aspects of Emacs like windows and frame, scrolling control,  buffer,
  navigation, opening files
  or web pages from file name or URL at point, numeric keypad handling,
  etc...  These files can be used independently as (*mostly*)
  independent Emacs Lisp *libraries* if you prefer to use the features
  without the PEL key bindings.

- Extensive documentation in the form of PDF files, one file
  per Emacs topics.
  Each table provides an overview, command descriptions, related
  key bindings  and links to related on-line documents.  The
  tables have extensive markup with colours for standard Emacs,
  external package, and PEL-provided command and key bindings.

PEL relies on Emacs customization system.  PEL activates third party
packages through `PEL customization`_, by setting a corresponding ``pel-use-...``
variable to t. Once a feature is activated through customization,
PEL also provides extra key bindings and in some cases allow dynamic
activation and de-activation of external packages.

PEL code is written in several files.  The pel.el file holds ``pel-init``
which initializes PEL, controls auto-loading of all supported packages
and builds the key bindings.  There are several other PEL files that
are used by that.  But they can also be used independently.  So if you
do not want to use PEL key bindings, you can just use some of the PEL
modules and provide you own bindings in your own Emacs init file.

PEL  integrates with a set of third party Emacs packages
(see the list in the `Credits`_ section below) and provides extra key bindings
to use the feature of those packages, sometime through extension functions
provided by PEL code.
In several cases PEL provides the logic to install these third party Emacs
packages, the logic to configure them and the logic to load them as lazily
as possible to reduce the Emacs initialization start time to a minimum.

The use of PEL features and PEL uses of other third party Emacs packages is
controlled by the `PEL customization`_.  The default customization leave
most packages un-activated. To use their features you must
first activate them via the `PEL Customization`_ mechanism.

To use the PEL auto-loading of packages and key bindings, put the
following code inside your Emacs ``init.el`` file:

.. code:: elisp

      (require 'pel)
      (pel-init)

You can place you own customizations after executing ``pel-init``.
This allows you to overwrite bindings done by PEL for instance, or
complement it.

To start or re-start PEL interactively, type::

  M-x pel-init


The reason for PEL
------------------

PEL attempts to make Emacs easier for new users by providing already made
configuration that is controlled by Emacs customization system.  It reduces the
need for writing Emacs Lisp configuration code for the packages it supports and
incorporates the knowledge inside files that can be byte-compiled for futher
speed enhancements.

Emacs supports a number of great packages. Some are easy to install, others
require more knowledge, knowledge that is often not readily available to new
users and will require a time investment you may not be willing to make.

Instead of having to write Emacs Lisp code inside an Emacs init file for each
new package you want to use, you'd use PEL, select the features you want
via `PEL Customization`_ and then execute ``pel-init`` to activate what you want
to use.  PEL contains the logic for configuring the packages it supports.  In
some cases it also contains the logic to install the package if it is not
already installed.

This is an early version of PEL. It will grow over time and will support more
packages. PEL essentially came out as a desire to be able to use an Emacs
configuration on several systems, both in terminal (TTY) mode and in Graphics
mode while trying to keep  Emacs initialization as fast as possible and reducing
the repetitive writing of package initialization code.

I started writing PEL while learning Emacs, Emacs Lisp and the amazing packages
that have been written for Emacs.  PEL encapsulates the knowledge about various
tweaks in the use and configuration of several built-in Emacs features and
several of the third party packages.

While learning Emacs and various packages I created a set of tables
that each list and describe a specific topic, the commands and key bindings
related to that topic.
There are several topics; Emacs navigation, Emacs
buffers, windows and frames, how to undo, redo, work with Emacs Lisp, etc...
See the `Key Binding Documentation`_ section.
The commands and key bindings described in those table include what is provided
by the plain vanilla GNU Emacs but also the bindings PEL adds and
the bindings provided by the third party packages that PEL integrates.


PEL Goals
---------

- Ease introduction to Emacs.
- Simplify and speed up Emacs configuration.
- Keep as many standard Emacs key bindings as possible.
- Provide easy to remember key bindings via a key binding tree, key prefixes and
  the use of key choice visualization with package such as which-key_, especially
  for commands that are seldom used.
- Minimize the amount of Emacs Lisp code to write inside Emacs init file to
  support various external Emacs packages.

  - Provide all logic necessary to install and configure external Emacs packages.

- Minimize Emacs initialization time even when a large number of packages are
  present on the computer.
- Document what's available: the key bindings, the special considerations, the
  documents that should be read to deepen user's understanding.
- Allow use of PEL even when someone has an extensive Emacs init file.
- Add support for several programming languages integrating many packages that
  support these programming languages.  Support for C, C++, Rust, Go,
  Python, Erlang, Elixir, Haskell, OCaml and several are planned
  (but... no schedule yet!).


Essentially, PEL is my first Emacs Lisp project.  I wrote it while learning
Emacs.  I keep using the documentation whenever I forgot the key bindings
of one of the many Emacs features.  There are still a lot of things to do to add
support for external packages and increase the customization. And even more to
transform the documentation format (see the `PDF Documentation`_ section for
that.)


Using Portions of PEL Manually
------------------------------

There's another, manual way to use portions of PEL.
PEL code is split across several files.
Its keymap and installation logic is located inside the `pel.el`_ file
exclusively.
If you only want to use the feature of one or several other files, then simply
use them and never call ``pel-init``.
You can then create any key binding that you wish by writing your own
initialization code.

..
   -----------------------------------------------------------------------------


How to Setup PEL
================

Updates to your Emacs Initialization file
-----------------------------------------

Unfortunately *some* Emacs Lisp code must be written to your
`Emacs initialization file`_, but that's mainly to setup how to download packages
that you might already have, and possibly 2 lines to require and initialize PEL.

Tricks to Increase your Emacs init time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL itself loads quickly.  But you can improve your overall Emacs initialization
time further by enclosing the entire code of your init.el file inside:

.. code:: elisp

          (let ((file-name-handler-alist nil)
                (gc-cons-threshold most-positive-fixnum))

            ;; all your initialization code goes here

          )

What the above does is to disable special file association handling and garbage
collection while Emacs processes your initialization code.


Configure How to Download Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, Emacs stores its persistent customization data inside your Emacs
init file.  If you want to store it somewhere else, you to add something like
the following code, which places it inside the file ``~/.emacs-custom.el``:

.. code:: elisp

          (setq custom-file "~/.emacs-custom.el")
          (load custom-file)

**Note**
   If you work inside several projects and each project requires different
   Emacs settings, you could use several customization files and activate them
   for each project, reducing the load time further.
   That provides another degree of freedom, along with Emacs directory local
   and file local variables.

To start PEL when Emacs Starts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want PEL to be available right after Emacs starts, write the following
inside your Emacs init file:

.. code:: elisp

          (require 'pel)
          (pel-init)

If you do not want PEL to start when Emacs start, then you don't need the above
code. To use PEL later simply execute the **pel-init** command by typing:
``M-x pel-init``


To identify the location of your Ispell local dictionary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the current version of PEL, when you want to select the spell check
program used by
Ispell or Flyspell and the location of your personal dictionary you need to
write Emacs Lisp code in your Emacs init file that calls the pel-spell-init
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

The following code re-assign the F6 key to `undo' and uses the F7 key
to be used as what PEL normally uses for F6:

.. code:: elisp

          (global-set-key (kbd "<f6>") 'undo)
          (global-set-key (kbd ("<f7>") pel:f6)



..
   -----------------------------------------------------------------------------

PEL Specific Features
=====================

PEL implements a set of small utilities that complement what's already available
in standard GNU Emacs and some other packages. The code is spread into several
small files.  Each of those file is described in the following subsections.
PEL comes with a set of PDF files that describe key bindings , including the
standard GNU Emacs bindings, the bindings of the external packahges integrated
by PEL and the bindings for PEL commands.  The sections below contain link to
the relevant PDF files.  The complete list of PDF files is shown in the
`Key Binding Documentation`_ section.

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
  buffer if it can.  Activation is not alloewed when Company mode is active
  for the current buffer.
- ``pel-global-company-mode`` toggles the global Company mode on/off if it
  can. Activation is not allowed when Auto Complete mode is active.
- ``pel-company-mode`` toggles the Company mode on/off for the current buffer if
  it can.  Activation is not allowed when Auto Complete mode is active for the
  current buffer.
- ``pel-completion-help`` shows the state of the auto completion global and
  buffer specific modes.  It displays if the packages are available and whether
  they are enabled on not.
- ``pel-complete`` performs an explicit completion using the competion mode
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
  - ``pel-comment-middle`` display/set the string used to continuen a comment.
  - ``pel-comment-end``    display/set the string used to end a comment.

- With ``pel-toggle-comment-auto-fill-only-comments``  you control whether
  automatic filling is done inside source code comments.
- The ``pel-delete-all-comments`` deletes all comments in current buffer.
  Use `narrowing`_ to reduce the area where comments are deleted.
- The ``pel-kill-all-comments`` kills all comments in current buffer.
  Each killed comment group is retained in the kill ring, as a separate kill
  ring entry.  That allows selective restoration of comments later with yank
  operations.  See the `Cut, Delete, Copy and Paste`_ document.


PEL CUA Mode Extension Utilities - *experimental*
-------------------------------------------------

:PDF Docs: *none*
:PEL Customization: **none**
:PEL Key Prefix: **none**

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
text, targetting specific syntax entities or other simpler parts.

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
  - The ``pel-delete-to-next-visible`` delete all whitespace charactares between
    point and the next non-whitespace character.

- The ``pel-mark-whole-line`` marks the complete current line excluding the line
  termination.

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
a numeric argument to add the adornment specified by the customizable
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
menu via the minibuffer.  The key prefix for these command bindings is ``<f11><f10>``.

.. _here: http://emacs.stackexchange.com/questions/31791/order-of-items-in-imenu?noredirect=1#comment48799_31791


PEL Navigation
--------------

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
- ``pel-remove-window-from-scroll-sync`` removes the currenbt window from the
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
  See `PEL Navigation`_.

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

- ``pel-toggle-case-fold-search`` toggles seach case sensitivity in the current
  buffer.
- ``pel-toggle-search-upper-case`` changes the sensitivity behaviour of yank in
  search prompt between the following:

  - *nil* : upper case don't force case sensitivity,
  - *t* : upper case force case sensitivity,
  - *not-yanks* : upper case force case sensitivity, and
    lower case text when yank in search minibuffer.

- ``pel-show-search-case-state`` displays the search behaviour in the current
  buffer.




PEL Speedbar Management
-----------------------

:PDF Docs: `Speedbar`_.
:PEL Customization: ``pel-use-speedbar``, ``pel-prefer-sr-speedbar-in-terminal``.
:PEL Key Prefix: **pel:speedbar** : ``<f11> S``

The file `pel-speedbar.el`_ manages the accessibility and use of Emacs speed-bars:
both Emacs native Speedbar and the `SR-Speedbar`_ external package.
When the ``pel-use-speedbar`` customize variable is set to **t** PEL provides
key bindings for activating the speed-bars and provide some management
facilities. As shown in the PDF `Speedbar`_ table, PEL's
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
    only if the customizable variable ``pel-prefer-sr-speedbar-in-terminal``
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

One of the goal of this file is to avoid loading either Ispell or flyspell
until they are actually required while providing a function that can
configure these utilities: ``pel-spell-init``.

To configure Ispell and Flyspell without forcing early loading of the Ispell
and flyspell libraries you can write something like the following inside your
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


PEL Text Filling and Justification Utilities
-------------------------------------------

:PDF Docs: `Filling and Justification`_.
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

- ``pel-insert-line`` inserts a (commented) line.  The lenght of the line is
  controlled by the ``pel-linelen`` customizable variable, whcih defaults to 77.
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
    splitting logic by selecting an orienation that takes the frame size
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
    minibuffer or any dedicated window.
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
         Future version of PEL will allow customization of the prefix keys and the
         ability to control whether **F5** is bound by PEL.

The best way to quickly see the list of PEL prefix key is right inside Emacs.
Type the prefix key (like **F11**) and then quickly type
either **C-h** or **F1**.
Emacs will open a ``*help*`` buffer that lists all keys available.  You can
navigate this buffer and follow the links to the described commands. To get the
list of the keys for a sub-prefix type it and again follow with
either **C-h** or **F1**.

The following table lists the **F11** keymap as an example.
As described in the `Naming Conventions`_ section the names in the binding
column that use the "pel:" prefix are sub key-maps.
The commands use the prefix "pel-".
As you can see some of the commands are accessible right after the **F11**
prefix, but there's a large number of sub-prefix following.
The keymap names were chosen to be as descriptive as possible and use keys that
mnemonically associate to the related concept if at all possible.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f11> TAB``                   pel:indent
``<f11> SPC``                   Prefix Command
``<f11> #``                     pel-toggle-mac-numlock
``<f11> $``                     pel:spell
``<f11> '``                     pel:bookMark
``<f11> +``                     pel-copy-marked-or-whole-line
``<f11> ,``                     pel:auto-completion
``<f11> -``                     pel:kill
``<f11> .``                     pel:mark
``<f11> 0``                     hl-line-mode
``<f11> ;``                     pel:comment
``<f11> =``                     pel:copy
``<f11> ?``                     pel:help
``<f11> C``                     pel:clipboard
``<f11> F``                     pel:frame
``<f11> S``                     pel:speedbar
``<f11> [``                     pel-cua-move-rectangle-left
``<f11> ]``                     pel-cua-move-rectangle-right
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
``<f11> y``                     yank-pop
``<f11> |``                     pel:scroll
``<f11> <C-S-down>``            pel-close-window-down
``<f11> <C-S-left>``            pel-close-window-left
``<f11> <C-S-right>``           pel-close-window-right
``<f11> <C-S-up>``              pel-close-window-up
``<f11> <C-down>``              pel-create-window-down
``<f11> <C-left>``              pel-create-window-left
``<f11> <C-right>``             pel-create-window-right
``<f11> <C-up>``                pel-create-window-up
``<f11> <M-left>``              pel-backward-syntaxchange-start
``<f11> <M-right>``             pel-forward-syntaxchange-start
``<f11> <C-f10>``               menu-bar-mode
``<f11> <down>``                windmove-down
``<f11> <f10>``                 pel:menu
``<f11> <f11>``                 pel-toggle-frame-fullscreen
``<f11> <f12>``                 xterm-mouse-mode
``<f11> <left>``                windmove-left
``<f11> <right>``               windmove-right
``<f11> <up>``                  windmove-up
=============================== ===========================================

PEL Mode Sensitive Key-maps
---------------------------

In the above table,
the ``<f11> SPC`` is a special case. It's the top key-map of all mode sensitive
key-maps.
PEL uses the **F12** as the key prefix for a keymap that contains
commands for the major mode of the current buffer.

For example, when the current buffer is using the ``rst-mode``
for `editing reStructuredText files`_,
the **F12** key has the following bindings.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     pel-rst-makelink
``<f12> g``                     pel-rst-goto-ref-bookmark
``<f12> s``                     pel-rst-set-ref-bookmark
=============================== ===========================================

However, when the current buffer uses Emacs-Lisp mode for working on Emacs Lisp
code,
the **F12** key has the following, different bindings.


=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     pel-find-thing-at-point
``<f12> D``                     toggle-debug-on-error
``<f12> a``                     pel:elisp-analyze
``<f12> c``                     pel:elisp-compile
``<f12> d``                     pel:elisp-debug
``<f12> e``                     pel:elisp-eval
``<f12> f``                     pel:elisp-function
``<f12> i``                     parinfer-auto-fix
``<f12> l``                     pel:elisp-lib
``<f12> m``                     pel:elisp-mode
=============================== ===========================================

If you edit a reStructuredText file and want to use one of the commands
available in the Emacs-Lisp key-map, then you can use the longer PEL key-map
that uses the ``<f11> SPC`` prefix.
The following table shows that for Emacs Lisp (abbreviated "elisp") you'd type
``<f11> SPC l`` to get to the same key-map that ``<f12>`` provides when you're
already using the Emacs-Lisp major mode.

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

This is a very early version of PEL.
Support for programming and markup languages is currently very sparse.
More to come.


.. _editing reStructuredText files: `PEL reStructuredText Support Utilities`_

Key Binding Documentation
-------------------------

PEL comes with a set of tables listing and describing both the standard Emacs
commands and key bindings for a given type of activity along with the extra
commands provided by PEL.
These tables are inside PDF documents.
See the `PDF Documentation`_ section for more info on why PDF files were used.
The tables have a format that is something between a quick sheet format and
a full blown manual.

Each PDF file holds a table that list commands related to a specific topic and
holds overview above a list of rows on:

#. the command name with several hyperlinks to the related section of the
   GNU Emacs manuals or other rappropriate resource
#. the key bindings for that command including:

   - the standard Emacs key bindings
   - the bindings for integrated packages
   - the bindings specific to PEL

#. the Emacs Lisp function form for the command, with the function name in
   bold and the arguments in Emacs help style
#. A description of the command, with lots of the text taken directly from
   Emacs help for what relates to the interactive use of the function but also
   with extra notes and references.

Several of these documents also a list of reference table listing relevant topics.
These references include hyperlinks to the relevant GNU
Emacs manuals but also to several sites devoted to Emacs including several
demonstration videos hosted on various platforms.

The tables are heavily marked up using colors and icons (actually Unicode
character symbols) to represent various concepts. For example key bindings that
do not work when Emacs is running in terminal (TTY) mode are displayed in
orange, commands that require external Emacs package are show in blue and use a the
package character (📦), etc...  The full list of conventions are listed in the
`Document Legend`_ table.  The list of tables follow below.
This is the very first release of PEL.
As PEL evolves, it will cover more topics, more
programming languages, major modes and will integrate with more of the external
Emacs packages and more tables will describe how to use them.

PDF Document Tables
~~~~~~~~~~~~~~~~~~~

- `Document Legend`_

**Emacs base operations:**

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
#. `File and Directory Local Variables`_
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
#. `Modifier Keys`_
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
.. _Enriched Text:                            pdf/enriched-text.pdf
.. _ERT:                                      pdf/ert.pdf
.. _Faces and Fonts:                          pdf/faces-fonts.pdf
.. _File Management:                          pdf/file-mngt.pdf
.. _File and Directory Local Variables:       pdf/file-variables.pdf
.. _Filling and Justification:                pdf/filling-justification.pdf
.. _Frames:                                   pdf/frames.pdf
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



PEL Customization
=================

PEL is heavily customizable using the `Emacs customization`_ facility.

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

       - It will only do that if you change PEL's custmization and re-run
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

pel-use-esup                   Enabless the use of the esup_                                 Yes, from MELPA_.
                               package, the Emacs StartUp Profiler.

pel-use-expand-region          Enabless the use of the                                       Yes, from MELPA_.
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
                               This is a *thinkering experiment* and is likely
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
                               with this.                                                    of Eamcs.

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
                               Speedbar inside the same frame, useful in terminal (tty)
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
                               which-key is used, so this variable is set to **t**
                               by default.
============================== ============================================================= =================

.. References

.. _ace-window package:        https://melpa.org/#/ace-window
.. _Auto Complete:
.. _auto-complete package:     https://melpa.org/#/auto-complete
.. _MELPA:                     https://melpa.org/
.. _use-package:               https://melpa.org/#/use-package
.. _bind-key:                  https://melpa.org/#/bind-key
.. _bm:                        https://melpa.org/#/bm
.. _c-eldoc:                   https://melpa.org/#/?q=c-eldoc
.. _SBCL:                      https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
.. _slime:                     https://melpa.org/#/slime
.. _slime package:             https://melpa.org/#/slime

.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Customization.html#Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
.. _ELPA:                      https://elpa.gnu.org
.. _company:                   https://melpa.org/#/company
.. _dired-narrow:              https://melpa.org/#/dired-narrow
.. _esup:                      https://melpa.org/#/esup
.. _expand-region:             https://melpa.org/#/expand-region
.. _framemove:                 https://www.emacswiki.org/emacs/FrameMove
.. _free-keys:                 https://melpa.org/#/free-keys
.. _goto-last-change:          https://melpa.org/#/goto-last-change
.. _graphviz-dot-mode:         https://melpa.org/#/graphviz-dot-mode
.. _highlight-defined:         https://melpa.org/#/highlight-defined
.. _lice:                      https://melpa.org/#/lice
.. _macrostep:                 https://melpa.org/#/macrostep
.. _nhexl-mode:                https://elpa.gnu.org/packages/nhexl-mode.html
.. _parinfer:                  https://melpa.org/#/parinfer
.. _rainbow-delimiters:        https://melpa.org/#/rainbow-delimiters
.. _popup-kill-ring:           https://melpa.org/#/popup-kill-ring
.. _rg:                        https://melpa.org/#/rg
.. _rust-mode:                 https://melpa.org/#/rust-mode
.. _racer:                     https://melpa.org/#/racer
.. _cargo:                     https://melpa.org/#/cargo
.. _sr-speedbar:               https://melpa.org/#/sr-speedbar
.. _undo-tree:                 https://elpa.gnu.org/packages/undo-tree.html
.. _which-key:                 https://elpa.gnu.org/packages/which-key.html






Implementation Notes
====================

Emacs Lisp Files
----------------

PEL code is placed in several Emacs Lisp files.

- The file `pel.el`_ defines the ``pel-init`` function, the only one
  autoloaded automatically by Emacs, as identified by the file
  `pel-autoloads.el`_.

  - When ``pel-init``  is executed it loads the `pel-zkeys.el`_
    file that contains all PEL key bindings as required by customization.

- All PEL customization variables are defined in the file `pel--options.el`_.
- Several low level utilities are defined inside the file `pel--base.el`_.
- The other Emacs Lisp files implement the PEL convenience features.
  These files are mostly independent from each other, only requiring, for most
  of them either nothing or `pel--base.el`_ only.
  PEL tries to load only what is needed, based on the commands that are
  executed. For that it implements its own auto-loading mechanism inside
  the file `pel-autoload.el`_, loaded only by ``pel-init``.
  The ``pel-init`` function calls ``pel--autoload-init`` which set the
  auto-loading of the PEL functions.
  This essentially implements a 2-step auto-loading mechanism.

  - As an example of one of the PEL convenience feature file,
    the file `pel-navigate.el`_ provides extra navigation facilities
    such as the use of multi-hit ``<home>`` and ``<end>`` keys similar to what is
    available by editors in the Brief family (such as CRiSP) but also aware of Emacs
    concepts such as text fields, `shift-key selection`_ and Emacs `mark and region`_.

- The `pel-pkg.el`_ defines the dependencies.  PEL only uses `use-package`_
  to control the installation of missing package and all other packages are only
  loaded if their feature is activated via `PEL customization`_.  However,
  when loading PEL via the standard Emacs package system, dependencies are
  identified becuase package needs the package to byte compile without
  generating warnings and errors.  This means that the third party packages will
  be installed on your disk when you install PEL via an Elpa compatible archive
  (such as MELPA_).  However:

  #. I did not yet start working on submitting this project on MELPA_, I'll do it
     once I feel PEL has enough to offer.
  #. You can just clone the project repo inside a directory and place this
     inside your Emacs load-path.  Running ``pel-init`` will then download the
     packages required by customization.

It's possible to use part of PEL without using its key bindings.
Just use the files that contain the features you need and write your own key
bindings for them inside your Emacs init file.  Just don't call ``pel-init``.

.. _build-pel.el:           ../build-pel.el
.. _pel.el:                 ../pel.el
.. _pel--options.el:        ../pel--options.el
.. _pel--base.el:           ../pel--base.el
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
.. _pel-zkeys:
.. _pel-zkeys.el:           ../pel-zkeys.el

.. _shift-key selection:  https://www.gnu.org/software/emacs/manual/html_node/emacs/Shift-Selection.html#Shift-Selection
.. _mark and region:      https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html#Mark


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



Credits
=======

PEL integrates with several great Emacs Lisp packages.  Some of them are
required, the others are used if they are present and are activated by the PEL
customization.

- PEL uses the following libraries distributed with GNU Emacs:

  - bookmark
  - cc-vars
  - cua-rect
  - delsel
  - elint
  - ert
  - flyspell
  - hl-line
  - imenu
  - isearch
  - ispell
  - kmacro
  - paragraphs
  - simple
  - subr-x
  - subword
  - thingatpt

- PEL has the following dependencies on the following external Emacs packages:

  - `use-package`_ 2.4, by John Wiegley, GPL V3.0.

- Finally PEL can integrate and use the following external Emacs packages, when
  they are activated by PEL customize variables:

  - ace-window
  - bind-key
  - erlang-flymake
  - erlang-start
  - popup
  - pos-tip
  - sr-speedbar

..
   -----------------------------------------------------------------------------
:PDF Docs: `Comments`_, `Cut, Delete, Copy and Paste`_, `Narrowing`_
..  LocalWords:  PEL

==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

.. [from:  README ]

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
- Key bindings using function key prefixes (F2, F6, F11 and F12)
  to provide easy access to many features and help learn what's available.

  - Most standard Emacs keys are left untouched.
  - The which-key package is used and activated by default, allowing
    you to see what's available easily.  F11 is the main prefix key
    and all prefixes have a meaningful name that starts with the
    'pel:' prefix.  F2 and F6 are used as global shortcut prefix keys,
    and F12 as a mode-sensitive shortcut prefix key.
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
packages through customization, by setting a corresponding ``pel-use-...``
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
controlled by the `PEL Customization`_.  The default customization leave
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

More information is available in `PEL's Manual`_.

.. [ endof: README ]

.. _PEL's Manual: doc/pel-manual.rst
.. _which-key:    https://elpa.gnu.org/packages/which-key.html
.. _Key Binding Documentation: doc/pel-manual.rst#pel-key-bindings
.. _Credits:            doc/pel-manual.rst#credits
.. _PEL Customization:  doc/pel-manual.rst#pel-customization

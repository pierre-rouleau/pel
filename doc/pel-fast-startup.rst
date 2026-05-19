=============================
PEL Fast Startup Mode
=============================

:URL: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-fast-startup.rst
:Project:  `PEL Project home page`_
:Created:  2026-05-19
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2026-05-19
:License:
    Copyright (c) 2026 Pierre Rouleau <prouleau001@gmail.com>

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

PEL fast startup mode is a mechanism that dramatically reduces Emacs startup
time when a large number of ELPA packages are installed.  The goal is to
bring the startup time of a fully-configured Emacs session with hundreds of
packages down from several seconds to under 300 milliseconds — and often
well under 200 milliseconds.

The general principle is to reorganize the ELPA package directory layout,
replacing the many per-package directories on ``load-path`` with a single
*bundle* directory that holds copies of the Emacs Lisp files from all
single-level packages.  This dramatically reduces the number of directories
that Emacs must search during startup.

PEL's fast startup mode can be activated and deactivated at any time using
interactive commands without deleting any installed package.  Switching modes
requires an Emacs restart but preserves all installed packages.

Design principles:

- **Non-destructive**: switching modes never deletes packages; files are
  reorganized rather than removed, so switching back to normal mode is safe.
- **Transparent to package.el**: the same ``~/.emacs.d/elpa`` symlink is used
  in both modes; package.el works without modification.
- **Multi-process safe**: changing modes from one Emacs session does not
  affect other running Emacs processes.
- **Compatible with package-quickstart**: fast startup mode works with the
  Emacs 27+ package quickstart mechanism for additional speed gains.

-----------------------------------------------------------------------------

The Problem: Large ``load-path``
=================================

When Emacs starts, the package manager (``package.el``) initializes by
scanning the ELPA directory (typically ``~/.emacs.d/elpa``).  It adds a
directory entry to ``load-path`` for *each* installed package.  With 200+
packages this can result in a ``load-path`` with over 200 entries.

During startup, every ``(require 'feature)`` and ``(load "file")`` call
consults the entire ``load-path``, doing filesystem operations for each
entry until the file is found.  A long ``load-path`` therefore multiplies
the number of I/O operations performed at startup.

PEL's fast startup mode solves this by bundling all single-directory ELPA
packages into one flat directory, reducing the effective ``load-path`` length
from hundreds of entries to fewer than twenty.

-----------------------------------------------------------------------------

Architecture Overview
======================

The fast startup mode works by:

1. Creating a *bundle* directory that contains copies of all Emacs Lisp files
   from packages that have a single directory (the majority of ELPA packages).

2. Creating an ``elpa-reduced`` directory that contains only the packages
   that have sub-directories (multi-directory packages, which cannot be
   bundled) plus the bundle directory.

3. Redirecting the ``elpa`` symlink to point to ``elpa-reduced`` instead of
   the original ``elpa-complete`` directory.

4. Generating a small ``pel-fast-startup-init.el`` file that configures
   package version information so that the multi-directory packages can find
   their dependencies in the bundle.

In fast startup mode Emacs sees only a handful of directories through
``package-user-dir``/``load-path``, rather than one directory per package.

-----------------------------------------------------------------------------

Directory Structure in ``~/.emacs.d``
=======================================

Normal Startup Mode
--------------------

In normal startup mode the directory layout looks like this::

    ~/.emacs.d/
    ├── early-init.el                    # Early init file (Emacs 27+)
    ├── init.el                          # Main Emacs init file
    ├── emacs-customization.el           # PEL customization file
    ├── elpa -> elpa-complete/           # Symlink (→ elpa-complete/)
    ├── elpa-complete/                   # All installed ELPA packages
    │   ├── ace-link-20210121.923/
    │   ├── ace-window-20210617.2224/
    │   ├── ...hundreds of packages...
    │   └── yasnippet-20200604.246/
    ├── utils/                           # PEL utility files (non-ELPA)
    │   ├── some-util.el
    │   └── ...
    └── pel_keys.elc                     # Byte-compiled PEL key binding file

The ``elpa`` directory is a symbolic link that points to ``elpa-complete``.
This indirection is the key mechanism that allows PEL to switch between
modes by simply retargeting the symlink.

Fast Startup Mode
-----------------

After running ``pel-setup-fast``, the directory layout becomes::

    ~/.emacs.d/
    ├── early-init.el                    # Same file (managed by PEL setup)
    ├── init.el                          # Same init file
    ├── emacs-customization.el           # Same customization file
    ├── pel-fast-startup-init.el         # GENERATED — presence signals fast mode
    ├── elpa -> elpa-reduced/            # Symlink NOW points to elpa-reduced
    ├── elpa-complete/                   # All packages, untouched
    │   ├── ace-link-20210121.923/
    │   ├── ...unchanged...
    │   └── yasnippet-20200604.246/
    ├── elpa-reduced/                    # Reduced package set
    │   ├── pel-bundle-20260519.1030/    # THE BUNDLE (one-level packages)
    │   │   ├── ace-link.el             # Copied from ace-link package
    │   │   ├── ace-link.elc
    │   │   ├── ace-window.el
    │   │   ├── ...all single-dir pkg files...
    │   │   ├── pel-bundle-pkg.el       # Bundle package metadata
    │   │   └── pel-bundle-autoloads.el # Consolidated autoload definitions
    │   ├── treemacs-20210606.1918/      # Multi-directory package: kept separate
    │   ├── lsp-mode-20210618.1525/      # Multi-directory package: kept separate
    │   └── ...other multi-dir packages...
    ├── utils/                           # PEL utilities: unchanged
    └── pel_keys.elc                     # Recompiled for fast startup mode

The presence of ``pel-fast-startup-init.el`` in ``~/.emacs.d`` is the signal
that fast startup mode is active.  Its absence means normal mode is in effect.

When Dual Environment is Enabled
---------------------------------

When dual environment mode is active (separate configurations for TTY and
graphics Emacs), additional ``-graphics`` variants of directories and files
are created::

    ~/.emacs.d/
    ├── emacs-customization.el           # TTY customization
    ├── emacs-customization-graphics.el  # Graphics customization
    ├── elpa -> elpa-complete/           # TTY symlink
    ├── elpa-graphics -> elpa-complete-graphics/  # Graphics symlink
    ├── elpa-complete/                   # TTY packages (complete)
    ├── elpa-complete-graphics/          # Graphics packages (complete)
    ├── elpa-reduced/                    # TTY packages (fast mode)
    └── elpa-reduced-graphics/           # Graphics packages (fast mode)

-----------------------------------------------------------------------------

Emacs Initialization Files
============================

PEL's fast startup mode involves several Emacs initialization files.  Some
are provided as templates by PEL, some are generated dynamically, and some
are the user's own files.

``~/.emacs.d/early-init.el``
-----------------------------

Introduced in Emacs 27, ``early-init.el`` runs before the package manager
and graphics system are initialized.  This is the key file for fast startup
performance because it can configure the package system before ``package.el``
starts scanning directories.

PEL provides a template at ``example/init/early-init.el`` that the user
copies to ``~/.emacs.d/early-init.el``.  PEL's setup commands automatically
update certain variables within this file.

**Startup optimizations available in ``early-init.el``:**

1. **GC threshold boost** (controlled by ``pel-early-init-support-gc-boost-p``):

   Temporarily raises ``gc-cons-threshold`` to ``most-positive-fixnum``
   during initialization to prevent Emacs from pausing for garbage collection
   while loading hundreds of Emacs Lisp files.  The original GC settings are
   restored via ``emacs-startup-hook`` after initialization completes.

   .. code:: elisp

     (when pel-early-init-support-gc-boost-p
       (setq gc-cons-threshold most-positive-fixnum
             gc-cons-percentage 0.6)
       (add-hook 'emacs-startup-hook
                 (lambda ()
                   (setq gc-cons-threshold pel--ei-gc-cons-threshold-saved
                         gc-cons-percentage pel--ei-gc-cons-percentage-saved))))

2. **File-name-handler suppression** (controlled by ``pel-early-init-suppress-file-name-handler-p``):

   Every ``load``/``require`` call consults ``file-name-handler-alist`` to
   check for special file handlers (remote files, compressed archives, TRAMP,
   etc.).  During startup PEL loads only local, uncompressed files, so
   these checks are unnecessary overhead.  Setting ``file-name-handler-alist``
   to ``nil`` during startup eliminates this overhead; the original value is
   restored via ``emacs-startup-hook``.

3. **UI element suppression** (controlled by ``pel-early-init-disable-ui-elements-p``):

   In graphics mode, Emacs renders tool bars, menu bars and scroll bars very
   early in the startup sequence.  If they are later disabled in ``init.el``,
   Emacs wastes time rendering and then tearing them down.  Setting these
   via ``default-frame-alist`` in ``early-init.el`` prevents the
   render-then-teardown cycle.

   Note: since ``display-graphic-p`` is not available during early-init,
   graphics mode is detected via environment variables.

4. **Dual environment support** (controlled by ``pel-early-init-support-dual-environment-p``):

   When dual environment is active and graphics mode is detected, advises
   ``package-load-all-descriptors`` to use the ``-graphics`` variant of
   ``package-user-dir``.

5. **Fast startup activation**:

   Checks whether ``pel-fast-startup-init.el`` exists in
   ``user-emacs-directory``.  If found, loads it and calls
   ``pel-fast-startup-init``.  This is the mechanism that activates PEL's
   fast startup mode.

   .. code:: elisp

     (let ((fast-startup-setup-fname
            (expand-file-name "pel-fast-startup-init.el"
                              user-emacs-directory)))
       (when (file-exists-p fast-startup-setup-fname)
         (when (load (file-name-sans-extension fast-startup-setup-fname)
                     :noerror :nomessage)
           (pel-fast-startup-init pel-force-graphic-specific-custom-file-p
                                  pel-early-init-support-package-quickstart-p)
           (defvar pel-running-in-fast-startup-p)
           (setq pel-running-in-fast-startup-p t))))

**Variables in ``early-init.el`` controlled by PEL setup commands:**

+--------------------------------------------------+----------+-------------------------------------------+
| Variable                                         | Default  | Purpose                                   |
+==================================================+==========+===========================================+
| ``pel-early-init-support-package-quickstart-p``  | ``nil``  | Enable package quickstart (Emacs 27+)     |
+--------------------------------------------------+----------+-------------------------------------------+
| ``pel-early-init-support-dual-environment-p``    | ``nil``  | Enable separate TTY/graphics environments |
+--------------------------------------------------+----------+-------------------------------------------+
| ``pel-early-init-shell-detection-envvar``        | ``"_"``  | Env var that indicates shell launch       |
+--------------------------------------------------+----------+-------------------------------------------+
| ``pel-early-init-support-gc-boost-p``            | ``nil``  | Enable GC boost during startup            |
+--------------------------------------------------+----------+-------------------------------------------+
| ``pel-early-init-suppress-file-name-handler-p``  | ``nil``  | Suppress file-name-handler during startup |
+--------------------------------------------------+----------+-------------------------------------------+
| ``pel-early-init-disable-ui-elements-p``         | ``nil``  | Disable UI rendering early (graphics)     |
+--------------------------------------------------+----------+-------------------------------------------+

**Computed constants in ``early-init.el``:**

- ``pel-force-graphic-specific-custom-file-p``: Non-nil when dual environment
  is enabled and Emacs is detected to be running in graphics mode.  Controls
  whether ``-graphics`` suffixed file names are used.
- ``pel--ei-in-graphics-p``: Non-nil when early-init detects graphics mode
  (via environment variables, since ``display-graphic-p`` is unavailable).

``~/.emacs.d/init.el``
------------------------

The main initialization file.  PEL provides a template at
``example/init/init.el`` that the user copies to ``~/.emacs.d/init.el``.

In fast startup mode ``init.el`` detects the mode by testing the value of
``pel-running-in-fast-startup-p`` (set by ``early-init.el``) and takes a
different initialization path:

- **Normal mode**: calls ``package-initialize``, which loads package
  descriptors for all packages in ``elpa-complete``, then calls PEL's
  initialization which may install missing packages.
- **Fast startup mode**: skips ``package-initialize`` (packages are already
  configured by ``pel-fast-startup-init``), defers certain initialization
  steps to ``emacs-startup-hook``, and does not attempt to install missing
  packages.

In both modes ``init.el`` also:

- Transforms ``package-user-dir`` to its ``file-truename`` to prevent
  breakage if the ``elpa`` symlink target changes while Emacs is running.
- Adjusts ``package-user-dir`` to the ``-graphics`` variant when dual
  environment is enabled and Emacs is in graphics mode.
- Saves the original ``package-user-dir`` in ``pel-package-user-dir-original``
  before modifying it (used by ``pel-locate-elpa`` later).

``~/.emacs.d/pel-fast-startup-init.el``  (generated)
------------------------------------------------------

This file is **dynamically generated** by PEL's ``pel-setup-fast`` command.
Its presence in ``~/.emacs.d`` signals that fast startup mode is active.
Its removal (by ``pel-setup-normal``) reverts Emacs to normal mode.

The generated file contains:

1. A helper function ``pel--add-to-load-path-once`` that adds a directory to
   ``load-path`` only if it is not already present.

2. A variable ``pel-fast-startup-builtin-packages`` holding a list of
   ``(package-symbol version-list)`` pairs for all packages that were bundled
   into ``pel-bundle``.  These are registered as "built-in" so that
   ``package.el`` does not complain about unresolvable dependencies.

3. The function ``pel-fast-startup-init`` which:

   - Registers all bundled packages into ``package--builtin-versions``.
   - Adds the ``pel-bundle`` directory to ``load-path`` (when package
     quickstart is enabled on Emacs 27+, so the quickstart file can find it).
   - Captures the original ``package-user-dir`` into
     ``pel-package-user-dir-original`` before any transformation, so that
     ``pel-locate-elpa`` works correctly in graphics mode.

4. An advice on ``package-compute-transaction`` that filters out any
   packages already known as built-in versions, preventing ``package.el``
   from requesting downloads of packages that are already bundled.

5. In dual environment mode: an advice on ``package-load-all-descriptors``
   to use the ``-graphics`` variant of ``package-user-dir``.

Example structure of the generated file:

.. code:: elisp

  ;; Generated by pel-setup-fast — do not edit by hand.
  ;; To regenerate, run pel-setup-fast again.

  (defvar pel-fast-startup-builtin-packages
    '((ace-link (0 1))
      (ace-window (0 1))
      ;; ...one entry per bundled package...
      (yasnippet (0 14 0)))
    "Packages bundled into pel-bundle; registered as built-ins.")

  (defun pel-fast-startup-init (&optional force-graphics using-package-quickstart)
    "Initialize fast startup mode."
    ;; Register bundled packages as built-ins
    (dolist (dep-ver pel-fast-startup-builtin-packages)
      (add-to-list 'package--builtin-versions dep-ver))
    ;; Preserve original package-user-dir before any transformation
    (unless (bound-and-true-p pel-package-user-dir-original)
      (when (boundp 'package-user-dir)
        (defvar pel-package-user-dir-original nil)
        (setq pel-package-user-dir-original package-user-dir)))
    ;; In package-quickstart mode add bundle to load-path
    (when using-package-quickstart
      (pel--add-to-load-path-once
       "/path/to/elpa-reduced/pel-bundle-YYYYMMDD-HHMM")))

  ;; Prevent downloads of already-bundled packages
  (defun pel--pct (result)
    "Filter out already-available packages from transaction RESULT."
    ...)
  (advice-add 'package-compute-transaction :filter-return #'pel--pct)

.. note::

   After running ``pel-setup-fast`` the first time, or after installing new
   packages in normal mode, you must run ``pel-setup-fast`` again to
   regenerate ``pel-fast-startup-init.el`` with the updated package list.

-----------------------------------------------------------------------------

The Bundle Mechanism
====================

The *bundle* is the central mechanism of PEL's fast startup mode.  It is a
single ELPA-compatible package directory (``pel-bundle-YYYYMMDD.HHMM``)
inside ``elpa-reduced`` that contains copies of all Emacs Lisp files from
packages with a single directory level.

Why Bundling Works
-------------------

The Emacs ``load-path`` is a list of directories.  When Emacs evaluates
``(require 'some-feature)`` it searches each directory in ``load-path`` for
a file named ``some-feature.el`` or ``some-feature.elc``.  With 200 packages
each in its own directory, ``load-path`` has 200+ entries and every file
lookup requires up to 200 filesystem probes.

By copying all single-directory package files into one bundle directory,
``load-path`` needs only one entry for the bundle (plus entries for any
multi-directory packages).  This reduces filesystem I/O dramatically.

What Goes into the Bundle
--------------------------

Only *single-directory packages* are bundled — packages whose ELPA directory
contains only Emacs Lisp files with no sub-directories.  The vast majority
of ELPA packages fall into this category.

Files excluded from the bundle:

- ``*-pkg.el`` package specification files (not needed at runtime in the bundle).
- Files whose base name appears in ``pel--bundle-excluded-basenames``
  (e.g. ``elpa.el``, a historical ELPA bootstrap script included by some
  packages as a developer artifact but not needed at runtime).

*Multi-directory packages* (those with sub-directories, such as ``treemacs``,
``lsp-mode``, ``geiser``) remain in ``elpa-reduced`` as their own directories
and continue to be managed by ``package.el`` normally.

Bundle Directory Contents
--------------------------

The bundle directory (e.g. ``pel-bundle-20260519.1030``) contains:

- **``pel-bundle-pkg.el``**: Package specification declaring ``pel-bundle``
  as a package with no external dependencies.  This lets ``package.el``
  recognize the bundle as a valid installed package.

- **``pel-bundle-autoloads.el``**: A consolidated autoloads file generated
  from all the bundled Emacs Lisp files.  This replaces the per-package
  ``*-autoloads.el`` files and provides all autoload definitions in a single
  file.  It is byte-compiled for faster loading.

- **All ``.el`` and ``.elc`` files** from every bundled package.  These are
  *copies* (not symlinks) so the bundle is self-contained.

Naming Convention
-----------------

The bundle directory name includes a timestamp
(``pel-bundle-YYYYMMDD.HHMM``) to make each setup unique.  When ``pel-setup-fast``
is run again (e.g. after installing new packages), a fresh bundle directory
is created with a new timestamp.

-----------------------------------------------------------------------------

PEL Source Files Involved
==========================

``pel-setup.el``
-----------------

The main file implementing the mode-switching commands.  It provides:

- ``pel-setup-fast``: Activates fast startup mode for all configured
  environments (TTY and optionally graphics).
- ``pel-setup-normal``: Reverts to normal startup mode.
- ``pel-setup-dual-environment``: Configures independent TTY/graphics
  environments.
- ``pel-setup-check-dual-environment``: Validates dual environment consistency.
- ``pel-setup-info-dual-environment``: Reports dual environment status.

The function ``pel-setup-fast`` orchestrates ~20 steps per environment:

1. Validates the ELPA directory structure.
2. Transforms the ``elpa`` directory into a symlink pointing to
   ``elpa-complete`` (if not already done).
3. Removes any previous ``pel-bundle`` and ``elpa-reduced`` directories.
4. Creates the ``pel-bundle`` directory and copies all single-level package
   files into it.
5. Generates ``pel-bundle-pkg.el`` and ``pel-bundle-autoloads.el``.
6. Byte-compiles ``pel-bundle-autoloads.el``.
7. Creates the ``elpa-reduced`` directory.
8. Copies multi-directory packages from ``elpa-complete`` to
   ``elpa-reduced``.
9. Moves ``pel-bundle`` into ``elpa-reduced`` with a timestamp in its name.
10. Generates ``pel-fast-startup-init.el`` with the list of bundled packages
    and the ``pel-fast-startup-init`` function.
11. Retargets the ``elpa`` symlink from ``elpa-complete`` to
    ``elpa-reduced``.
12. Byte-compiles ``pel_keys.el`` with
    ``pel-running-in-fast-startup-p`` set to ``t``.
13. Optionally generates a package quickstart file (Emacs 27+).

The function also contains ``pel-setup-fast-startup-init-text``, which
produces the text content of the generated ``pel-fast-startup-init.el`` file.

``pel-setup-base.el``
----------------------

Base utility functions used by all PEL setup operations.  Contains helpers
for path manipulation, file operations, and symlink management.

``pel-setup-27.el``
--------------------

Emacs 27+ specific setup code.  Provides functions for managing the
``package-quickstart`` feature:

- Creating and updating the package quickstart file.
- Modifying ``early-init.el`` to enable/disable package quickstart.
- Handling the ``-graphics`` variants of quickstart files in dual
  environment mode.

``pel-package.el``
-------------------

Package management and statistics.  Key functions related to fast startup:

- ``pel-package-info``: Displays package statistics.
- ``pel-package-info-all``: Loads all PEL files and prints comprehensive
  statistics (used by the Makefile ``make stats`` target).
- ``pel-cleanup``: Removes unused packages from the ELPA directory, moving
  them to ``elpa-attic``.
- ``pel-clean-renamed-packages``: Detects and retires packages that have been
  renamed (e.g. ``go-translate`` renamed to ``gt``), moving the old package
  directory to ``elpa-attic`` to prevent filename collisions in the bundle.
- ``pel--elpa-package-renames``: Constant alist mapping old ELPA package
  symbols to their replacement names.

``pel-elpa.el``
----------------

ELPA directory manipulation utilities.  Key functions:

- ``pel-elpa-create-copies``: Copies ``.el`` and ``.elc`` files from
  single-level ELPA packages into the bundle directory.  Respects the
  ``pel--bundle-excluded-basenames`` exclusion list to skip files like
  ``elpa.el`` that should not be bundled.
- ``pel--bundle-excluded-basenames``: Constant list of file base names
  (without extension) to exclude from the bundle.
- Functions for managing the ELPA directory structure, attic directories,
  and symlinks.

-----------------------------------------------------------------------------

Startup Phases
===============

This section describes what happens at each phase of Emacs startup in both
normal and fast startup modes.

Phase 1 — Early Init (Emacs 27+)
----------------------------------

Both modes: ``early-init.el`` is loaded.

**Normal mode:**

- Optional startup optimizations run (GC boost, file-handler suppression,
  UI element suppression) based on the ``pel-early-init-*`` defconst values.
- ``pel-fast-startup-init.el`` does **not** exist → fast startup block is skipped.
- Emacs proceeds to its normal package initialization phase.

**Fast startup mode:**

- Same optional startup optimizations run.
- ``pel-fast-startup-init.el`` **exists** → it is loaded.
- ``pel-fast-startup-init`` is called with the force-graphics and
  quickstart flags.
- ``pel-running-in-fast-startup-p`` is set to ``t``.

Phase 2 — Package Manager Initialization
-----------------------------------------

This phase happens automatically between ``early-init.el`` and ``init.el``
in Emacs 27+.  In Emacs 26, package initialization happens when ``init.el``
calls ``(package-initialize)``.

**Normal mode:**

- ``package.el`` scans all directories in ``elpa-complete``.
- One ``load-path`` entry is added per package (200+ entries for a large setup).
- All package descriptors and autoloads are loaded.
- This phase can take seconds with many packages.

**Fast startup mode:**

- ``package.el`` scans directories in ``elpa-reduced``.
- Only the bundle directory and multi-directory packages are seen.
- One ``load-path`` entry is added for ``pel-bundle-TIMESTAMP`` (covering
  all single-directory packages) plus one per multi-directory package.
- The advice on ``package-compute-transaction`` prevents requests to
  download already-bundled packages.
- This phase takes a fraction of the time.

Phase 3 — ``init.el``
-----------------------

**Normal mode:**

- PEL checks user-options (``pel-use-*`` variables) and installs any
  missing packages.
- Full PEL initialization runs.
- GC settings and file-name-handler-alist are restored (if boosted/suppressed).

**Fast startup mode:**

- PEL detects fast mode (via ``pel-running-in-fast-startup-p``).
- PEL's automatic package installation is disabled.
- The initialization path is shorter and faster.
- GC settings and file-name-handler-alist are restored.

-----------------------------------------------------------------------------

Early-Init Optimizations Detail
=================================

GC Threshold Boost
------------------

Emacs uses garbage collection to reclaim memory.  The default GC threshold
triggers collection relatively frequently, which causes pauses during the
intensive file loading of Emacs startup.

By setting ``gc-cons-threshold`` to ``most-positive-fixnum`` in early-init,
garbage collection is effectively disabled during the startup sequence.
After initialization completes, the hook ``emacs-startup-hook`` restores the
original threshold values.

The speedup from this technique depends on how many Emacs Lisp files are
loaded during startup.  With hundreds of packages it can be significant.

To enable this optimization, set the user-option
``pel-early-init-support-gc-boost-p`` to ``t`` and run ``pel-setup-fast``
(or ``pel-setup-normal``) to apply it to ``early-init.el``.

File-Name-Handler Suppression
------------------------------

Emacs consults ``file-name-handler-alist`` for every file access to check
whether a special handler is registered (for remote files, compressed
archives, TRAMP connections, etc.).  During startup, PEL loads only local
uncompressed ``.el`` and ``.elc`` files, so these checks are redundant.

Setting ``file-name-handler-alist`` to ``nil`` during startup saves the
overhead of this repeated lookup.  The original value is restored by
``emacs-startup-hook``.

To enable this optimization, set
``pel-early-init-suppress-file-name-handler-p`` to ``t``.

UI Element Suppression
-----------------------

In graphics mode, the Emacs C layer initializes the tool bar, menu bar, and
scroll bar very early.  If ``init.el`` later calls ``(tool-bar-mode -1)``
and ``(menu-bar-mode -1)``, Emacs has already spent time rendering these
elements and must then destroy them.

Setting suppression entries in ``default-frame-alist`` in ``early-init.el``
prevents the initial render, saving the render-and-teardown cycle.

Note: ``display-graphic-p`` is not available during early-init; graphics mode
is detected using the environment variable heuristic (``PEL_EMACS_IN_GRAPHICS``
or absence of the shell-detection environment variable).

To enable this optimization, set ``pel-early-init-disable-ui-elements-p``
to ``t``.

-----------------------------------------------------------------------------

Dual Environment Support
=========================

PEL supports two independent Emacs configurations: one for terminal (TTY)
mode and one for graphics mode.  This is useful on systems like macOS where
the terminal and graphics versions of Emacs have different performance
characteristics and where users want to configure each mode differently.

Configuration Files
--------------------

In dual environment mode each environment has its own customization file:

- TTY: ``~/.emacs.d/emacs-customization.el``
- Graphics: ``~/.emacs.d/emacs-customization-graphics.el``

ELPA Directories
-----------------

Each environment also maintains its own ELPA directory structure:

- TTY normal mode: ``elpa`` → ``elpa-complete``
- TTY fast mode:   ``elpa`` → ``elpa-reduced``
- Graphics normal mode: ``elpa-graphics`` → ``elpa-complete-graphics``
- Graphics fast mode:   ``elpa-graphics`` → ``elpa-reduced-graphics``

When ``pel-setup-fast`` or ``pel-setup-normal`` is run, it operates on
**both** environments simultaneously, regardless of whether Emacs is currently
running in TTY or graphics mode.

Graphics Mode Detection in Early-Init
--------------------------------------

Since ``display-graphic-p`` is not available when ``early-init.el`` runs,
PEL uses an environment variable heuristic:

- If the environment variable ``PEL_EMACS_IN_GRAPHICS`` is set to ``"1"``,
  Emacs is in graphics mode.
- If the environment variable named by ``pel-early-init-shell-detection-envvar``
  (default: ``"_"``, the Bash last-argument variable) is **absent**, Emacs was
  launched from a GUI application launcher (not a shell), which implies graphics
  mode.

This heuristic is implemented in the computed constant ``pel--ei-in-graphics-p``
in ``early-init.el``.

Activating Dual Environment
-----------------------------

Run the command ``pel-setup-dual-environment`` once to create the necessary
directory structure and file variants.  This command:

- Creates ``emacs-customization-graphics.el`` as a copy of
  ``emacs-customization.el``.
- Creates ``elpa-complete-graphics`` as a copy of ``elpa-complete``.
- Updates ``early-init.el`` to enable dual environment support.

After running this command, restart Emacs for the changes to take effect.

-----------------------------------------------------------------------------

Package Quickstart Integration (Emacs 27+)
===========================================

Emacs 27 introduced the ``package-quickstart`` feature.  When enabled,
``package.el`` pre-generates a file containing all package activation code
and autoloads.  Emacs then loads this single file at startup instead of
scanning all package directories.

PEL integrates package quickstart with its fast startup mode for additional
speed gains.

How It Works with Fast Startup
--------------------------------

When both fast startup and package quickstart are enabled:

1. ``pel-setup-fast`` generates a ``package-quickstart.el`` file (via
   ``pel-setup-27.el``) reflecting the bundled package layout.
2. ``early-init.el`` sets ``package-quickstart`` to ``t``.
3. At startup, Emacs loads the pre-generated quickstart file instead of
   scanning the ``elpa-reduced`` directory.
4. ``pel-fast-startup-init`` adds the ``pel-bundle`` directory to
   ``load-path`` (so that the quickstart file can reference bundle files).

Limitations
-----------

- Package quickstart requires knowing the customization file path at
  early-init time (before customization variables are loaded).  This path
  is stored in ``pel-early-init-custom-file`` in ``early-init.el``.
- When using dual environment with package quickstart, the graphics-mode
  quickstart file must be set up separately.
- After installing or removing packages in normal mode, ``pel-setup-fast``
  must be run again to regenerate both the bundle and the quickstart file.

-----------------------------------------------------------------------------

Package Cleanup and Maintenance
================================

The PEL fast startup bundle requires that all package filenames within the
bundle are unique.  This can be violated in two ways:

1. **Renamed packages**: If a package is renamed (e.g. ``go-translate`` → ``gt``)
   and both the old and new package directories exist, their files may collide.

2. **Development artifact files**: Some packages include files like ``elpa.el``
   (the historical ELPA bootstrap script) as development artifacts.  Multiple
   packages shipping the same filename cause harmless but confusing warnings.

The ``pel-cleanup`` Command
---------------------------

The ``pel-cleanup`` command removes unused packages from the ELPA directory,
moving them to an ``elpa-attic`` directory.  It reports what was moved or
would be moved (in dry-run mode).

As of PEL 0.4.2, ``pel-cleanup`` also handles renamed packages:

- ``pel--elpa-package-renames``: An alist of known renames, e.g.
  ``'((go-translate . gt))``.
- ``pel-clean-renamed-packages``: Called during cleanup to detect when both
  the old and new package are present, and move the old one to
  ``elpa-attic``.  Also removes the old package symbol from
  ``package-selected-packages`` so Emacs no longer tries to activate it.

The ``pel--bundle-excluded-basenames`` Constant
------------------------------------------------

This constant (in ``pel-elpa.el``) lists file base names that should never
be copied into the bundle, even when they appear in ELPA packages:

- ``"elpa"``: The original ELPA bootstrap script (Tom Tromey, circa 2009),
  included by packages such as ``ivy`` and ``lispy`` as a developer
  convenience artifact.  No package requires it via ``(require 'elpa)`` at
  runtime.

Files on this list are silently skipped during bundle creation, preventing
the "Duplicate file name in bundle" warning.

-----------------------------------------------------------------------------

Switching Between Modes
========================

Activating Fast Startup Mode
-----------------------------

1. Make sure all desired packages are installed in normal mode.
2. Optionally run ``pel-cleanup`` to remove unused packages.
3. If renamed packages are present, ``pel-cleanup`` will retire the old ones.
4. Run ``M-x pel-setup-fast``.
5. Restart Emacs.

After the restart, Emacs will start in fast mode using ``elpa-reduced``.

Reverting to Normal Mode
-------------------------

1. Run ``M-x pel-setup-normal``.
2. Restart Emacs.

After the restart, Emacs will start in normal mode using ``elpa-complete``.
Package management is fully restored (PEL can install and remove packages).

Refreshing Fast Startup after Installing Packages
--------------------------------------------------

If you install new packages while in normal mode and then want to return to
fast mode:

1. Run ``M-x pel-setup-normal`` to ensure you are in normal mode.
2. Restart Emacs in normal mode.
3. Install the desired new packages.
4. Run ``M-x pel-setup-fast``.
5. Restart Emacs in fast mode.

Alternatively, while staying in normal mode you can run ``pel-setup-fast``
directly; PEL will regenerate the bundle with the newly installed packages.

.. note::

   You must restart Emacs after switching modes.  The mode switch modifies
   the ``elpa`` symlink target and generates/removes ``pel-fast-startup-init.el``,
   but the running Emacs process continues to use the layout it started with.

-----------------------------------------------------------------------------

Variable and Function Reference
=================================

Key Variables
--------------

``pel-running-in-fast-startup-p``
  Boolean.  Set to ``t`` by ``early-init.el`` when ``pel-fast-startup-init.el``
  is successfully loaded and the fast startup function is called.  Tested by
  ``init.el`` and ``pel_keys.el`` to take the fast-startup code path.

``pel-fast-startup-init-fname``
  The path ``~/.emacs.d/pel-fast-startup-init.el``.  Used by ``init.el`` to
  check for fast startup mode on Emacs 26 (where ``early-init.el`` is not
  available).

``pel-package-user-dir-original``
  Stores the value of ``package-user-dir`` before PEL modifies it.  Used by
  ``pel-locate-elpa`` to find the actual ELPA directory even after
  ``package-user-dir`` has been adjusted for dual environment mode.

``pel-early-init-support-gc-boost-p``
  Defconst in ``early-init.el``, controlled by PEL setup commands.  When
  ``t``, raises the GC threshold during startup.

``pel-early-init-suppress-file-name-handler-p``
  Defconst in ``early-init.el``.  When ``t``, suppresses
  ``file-name-handler-alist`` during startup.

``pel-early-init-disable-ui-elements-p``
  Defconst in ``early-init.el``.  When ``t``, prevents early rendering of
  tool bars, menu bars and scroll bars in graphics mode.

``pel-early-init-support-dual-environment-p``
  Defconst in ``early-init.el``.  When ``t``, enables independent TTY/graphics
  configurations.

``pel-early-init-support-package-quickstart-p``
  Defconst in ``early-init.el``.  When ``t``, enables Emacs 27+
  package quickstart.

``pel-force-graphic-specific-custom-file-p``
  Computed defconst in ``early-init.el``.  Non-nil when dual environment is
  active and graphics mode is detected.

``pel--elpa-package-renames``
  Defconst in ``pel-package.el``.  Alist of ``(old-package . new-package)``
  pairs for ELPA packages that have been renamed.

``pel--bundle-excluded-basenames``
  Defconst in ``pel-elpa.el``.  List of file base names to exclude from the
  bundle.

Key Functions and Commands
---------------------------

``pel-setup-fast``  (command)
  Activates fast startup mode.  Creates ``elpa-reduced``, builds the bundle,
  generates ``pel-fast-startup-init.el``, and redirects the ``elpa`` symlink.

``pel-setup-normal``  (command)
  Reverts to normal startup mode.  Redirects ``elpa`` back to
  ``elpa-complete`` and removes ``pel-fast-startup-init.el``.

``pel-setup-dual-environment``  (command)
  Sets up independent TTY and graphics Emacs environments.

``pel-fast-startup-init``  (function, in generated file)
  Called by ``early-init.el``.  Registers bundled packages as built-ins and
  optionally adds the bundle to ``load-path``.

``pel-elpa-create-copies``  (function)
  Copies single-level package files into the bundle directory, respecting
  the exclusion list.

``pel-cleanup``  (command)
  Removes unused packages from ELPA to ``elpa-attic``, and retires renamed
  packages.

``pel-clean-renamed-packages``  (function)
  Detects and moves superseded (renamed) ELPA packages to ``elpa-attic``.

``pel-package-info-all``  (command)
  Loads all PEL files and prints comprehensive package statistics.  Ensures
  ``package-activate-all`` (or ``package-initialize`` on Emacs 26) is called
  before gathering stats to produce accurate counts in batch mode.

``pel-package-info-message``  (function)
  Prints PEL package information to stdout; used by the Makefile
  ``make stats`` target.

-----------------------------------------------------------------------------

See Also
=========

- `doc/emacs-startup-time.rst`_ — Historical research document describing
  the experiments and measurements that led to the fast startup architecture.
- `PEL Manual`_ — Comprehensive PEL documentation.

.. _doc/emacs-startup-time.rst: https://github.com/pierre-rouleau/pel/blob/master/doc/emacs-startup-time.rst
.. _PEL Manual: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst

-----------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

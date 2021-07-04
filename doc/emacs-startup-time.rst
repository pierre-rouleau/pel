=========================
Speeding Up Emacs Startup
=========================

:Home URL:
:Project:
:Created:  Friday, July  2 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2021-07-02 10:30:28, by Pierre Rouleau.
:Copyright: © 2021, Pierre Rouleau

This document will contain information on speeding up Emacs startup further
than what is currently done by PEL. This is work in progress. 🚧

.. contents::  **Table of Contents**
.. sectnum::

.. ---------------------------------------------------------------------------

Background
==========

Speeding up Emacs startup is a controversial topic in the Emacs development
community. The reasons are varied but are often rooted to the facilities
provided by Emacs to work-around a slow Emacs startup.  These include:

- using Emacs as a daemon
- building an Emacs image with a large amount of often used packages,
- Emacs 27 and later package-quickstart mechanism.


Although these facilities are really helping and do provide the ability to
start editing files very quickly, they appeal to a set of use case scenarios,
but not all of them.

I often use multiple instances of Emacs for doing various, unrelated tasks and
want to keep each one a single entity with its own history and context.  I
also want to develop Emacs Lisp code and try it inside on or several Emacs
instances without affecting some already running Emacs sessions.  Because of
that I end up launching Emacs sessions relatively often, at least the ones
that are short-lived. And I want my Emacs sessions to start quickly, ideally
under 300 milliseconds.

Also, Emacs itself is improving.  The gccemacs_ implementation speed it up even
further.  But even with all of the existing techniques when a user has a large
number of Emacs package managed with the Emacs built-in package manager,
package.el, Emacs startup can slow down noticeably.

I started writing PEL as the mechanism I use to control what package I use and
allow me to quickly deactivate some of them.  PEL uses Emacs customization
mechanism and with that I can create several customization files, each one
specific to a given type of development or workflow.  I have managed to keep
my Emacs startup relative fast, despite the large number of packages that I
have downloaded, which is over 250.

But still, I'd like to see my Emacs start as quickly as other editors.  It
bugs me. I like Emacs.  I have invested time in learning Emacs. I know there
are much more to learn and I will continue this learning for a long time.  And
want to have a snappy startup for what I use every day, every hour.

I will write my ideas in this document as I go about trying to make my copy of
Emacs start as fast as possible while using a large number of packages.

Current Status
==============

Currently PEL uses:

- the built-in package manager, package.el,
- the built-in Emacs customization system,
- the Emacs Elpa directory where all packages installed by package.el are
  stored, each one into its own directory,
- a separate directory, the PEL Utils directory (normally in
  ``~/.emacs.d/utils``) that stores Emacs Lisp files that are not Elpa
  packages
- **pel-init** that controls installation and checks what must be activated as
  requested by the set of ``pel-use-`` user-options,

  - the PEL installation and activation logic attempts to delay activation as
    late as possible to ensure that the user pays in time for a feature only
    when that feature is first required.

- **pel-cleanup** that removes what is currently not used but installed.


I originally started measuring the startup with the benchmark-init package. As
I write this, in a remote access terminal-based session to Emacs 26.3 running
inside a 2014 macOS machine running a 4GHz Intel Core i7, benchmark-init is
reporting a 0.5 second Emacs startup time.
The benchmark-init system reports the following::


    ╼►[benchmark-init/root nil 287ms]
      ├─[default load 15ms]
      ├─[pel_keys load 34ms]
      │ ├─[thingatpt require 8ms]
      │ ├─[recentf require 10ms]
      │ │ ├─[~/.emacs.d/recentf load 0ms]
      │ │ ╰─[tree-widget require 8ms]
      │ │   ╰─[wid-edit require 9ms]
      │ ├─[speedbar require 11ms]
      │ │ ├─[sb-image require 7ms]
      │ │ │ ╰─[ezimage require 7ms]
      │ │ │   ╰─[image require 8ms]
      │ │ ╰─[dframe require 8ms]
      │ ├─[ls-lisp require 8ms]
      │ ├─[pel-autoload require 1ms]
      │ ├─[pel--keys-macros require 1ms]
      │ │ ╰─[pel--options require 9ms]
      │ ├─[pel--macros require 0ms]
      │ ╰─[pel--base require 1ms]
      ├─[pel require 0ms]
      ├─[~/.emacs.d/emacs-customization.el load 3ms]
      ├─[finder-inf require 9ms]
      ├─[~/.emacs.d/elpa/ace-link-20210121.923/ace-link-autoloads load 0ms]

      ...

      ├─[~/.emacs.d/elpa/windresize-0.1/windresize-autoloads load 0ms]
      ├─[info require 7ms]
      ├─[~/.emacs.d/elpa/with-editor-20210319.1930/with-editor-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/xcscope-20201025.2002/xcscope-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/xr-1.21/xr-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/yafolding-20200119.1353/yafolding-autoloads load 2ms]
      │ ╰─[kmacro require 2ms]
      ├─[~/.emacs.d/elpa/yaml-mode-20201109.1026/yaml-mode-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/yasnippet-snippets-20210105.1346/yasnippet-snippets-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/yasnippet-20200604.246/yasnippet-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/zoutline-20190520.1819/zoutline-autoloads load 0ms]
      ├─[~/.emacs.d/elpa/ztree-20210215.2111/ztree-autoloads load 1ms]
      ╰─[package require 12ms]
        ├─[epg-config require 1ms]
        ├─[url-handlers require 2ms]
        │ ╰─[url-parse require 1ms]
        │   ├─[auth-source require 4ms]
        │   │ ├─[eieio require 3ms]
        │   │ │ ╰─[eieio-core require 2ms]
        │   │ │   ├─[cl-macs require 2ms]
        │   │ │   ╰─[eieio-loaddefs require 2ms]
        │   │ ╰─[password-cache require 1ms]
        │   ╰─[url-vars require 1ms]
        ╰─[seq require 12ms]
          ╰─[cconv require 2ms]

There's 240 lines in this report. I removed several showing the loads from the
elpa directories that report as 0 millisecond.

On Emacs 27, the report is shorter and Emacs starts a little faster with the
package-quickstart activated.

The benchmark-init and make timeit
----------------------------------

Recently I added a ``timeit`` rule in PEL Makefile. This measures the time it
takes to start and stop Emacs.  This provides a better measurement of the
time, but without providing the insight available in benchmark-init report.
As the report show, benchmark-init itself takes time.

Currently, the ``make timeit`` report, on the same machine and environment,
for the 0.5 second reported by benchmark-init I get the following reports::

    >Pierres-iMac@Fri Jul 02@12:03:32[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 208 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.14
    user         0.03
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.15
    user         0.03
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.67
    user         1.01
    sys          0.54
    >Pierres-iMac@Fri Jul 02@12:03:43[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 208 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.14
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.15
    user         0.03
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.77
    user         1.07
    sys          0.57
    >


The computer is running lots of other applications and processes at the same
time.  The time measurement is therefore not very precise, but it gives an
indication.  Two consecutive ``make timeit`` runs report a time of 1.67 and
1.77 seconds respectively of which about two third is spent in user space and
one third in kernel space.

Could that be reduced?

I disabled the following lines in my init.el file that activate the benchmark-init:

.. code:: lisp

  (require 'benchmark-init
           (expand-file-name
            "~/.emacs.d/elpa/benchmark-init-20150905.938/benchmark-init"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)


And then, running ``make timeit`` again, I get a little faster startup of
about 1.65 second::

    >Pierres-iMac@Fri Jul 02@12:16:40[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 208 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.14
    user         0.03
    sys          0.02
    time -p emacs -nw -e kill-emacs
    real         1.66
    user         1.00
    sys          0.54
    >Pierres-iMac@Fri Jul 02@12:16:49[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 208 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.15
    user         0.03
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.64
    user         0.99
    sys          0.53
    >


Experiment - Adding a directory with symlinks to files
------------------------------------------------------

As a next step for trying to reduce the startup time, I removed the
benchmark-init directory located in
``~/.emacs.d/elpa/benchmark-init-20150905.938/benchmark-init`` and then added
another one, ``~/.emacs.d/elpa-copy-link`` to the front of load-path with the
following Emacs Lisp line placed inside my init.el file:

.. code:: lisp

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa-copy-link"))

Now, the ``make timeit`` test produces a little faster startup of about 1.53 seconds::

    >Pierres-iMac@Fri Jul 02@13:25:09[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 207 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.14
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.15
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.54
    user         0.93
    sys          0.49
    >Pierres-iMac@Fri Jul 02@13:25:18[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 207 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 241 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 ( 45 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.14
    user         0.03
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.14
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.52
    user         0.93
    sys          0.48
    >

I suspect the startup to be faster because the package.el initialization has
to parse all directories inside the ``~/.emacs.d/elpa`` directory.  With a
smaller number of directories or with one that provides symlinks to a lot of
Emacs Lisp files from a large number of packages, the search is faster.


Experiment - Disabling package.el
---------------------------------

Next I disabled setup and initialization of package.el altogether.
Because PEL depends on package.el to check for package presence and to install
requested but missing Elpa packages, I also had to disabled the
**pel-elpa-pkg-dependencies** and the **pel-package-installed-p**.  They
return hard-coded values that identify no dependencies and that the feature is
installed respectively, circumventing PEL logic that tries to manage
installation of missing packages.

The load-path holds the "~/.emacs.d/elpa-copy-link" directory at the beginning
of the list.  That directory contains symlinks to all Emacs Lisp files that
come from packages that have no sub-directories. For me, at this point,
that's 182 packages.  The directories **are still inside the
``~/.emacs.d/elpa`` directory** though.


With these changes, PEL driven Emacs is not able to install anything and
**pel-cleanup** does not work properly, but Emacs and its installed packages
is fully functional (although I can't request the installation of anything
that's not already installed.)

The impact on Emacs startup is negligible, and not significant, it's even a
little slower (so the difference is probably not significant) ::

    >Pierres-iMac@Fri Jul 02@13:33:18[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    - 207 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         1.63
    user         1.02
    sys          0.50
    >


Experiment - Disabling package.el and removing elpa directories
---------------------------------------------------------------

Keeping the same setup as the previous experiment I removed all directories of
``~/.emacs.d/elpa`` that contain no sub-directories and whose files are
identified by symlinks stored inside the ``~/.emacs.d/elpa-copy-link`` which
is in the load-path.

I tried that and Emacs **fails**!  Hum...

Now instead of of using symlinks inside the ``~/.emacs.d/elpa-copy-link``
directory I store a **copy** of the Emacs Lisp and their byte-compiled files
inside ``~/.emacs.d/elpa-copy-link``.

Now Emacs runs file and the speedup is
considerable: the `make timeit`` test shows 0.64 second instead of over 1.7
seconds!  That shaved about 1 full second out of the process!!

::

    >Pierres-iMac@Fri Jul 02@13:49:47[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    -  27 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.14
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         0.64
    user         0.46
    sys          0.07
    >Pierres-iMac@Fri Jul 02@13:49:57[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    -  27 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         0.64
    user         0.46
    sys          0.07
    >

Also the **emacs-init-time** command now returns 0.2 second!!

But wait....  The auto-loading was not done and packages that depend on it
do not work.

So the next step is to find a way to process the auto-loading of these files.
Instead of leaving all of the auto-loading files the way they are, since a
mechanism needs to be set in place to have the files located somewhere else,
I'll see if I can write something that will process all auto-loading files and
create a shorter piece of code that will run faster. I would probably
eliminate the auto-loading of docstrings to speed things up a little more.


Hopefully the resulting code will not take much time and we can keep the same
type of execution speed as what is shown above.


.. ---------------------------------------------------------------------------

Ideas to Investigate
====================

Processing the autoload cookies of files stored inside elpa-copy directory
--------------------------------------------------------------------------

The final system will most probably have to identify all things that need to
be auto-loaded by parsing the files in the ``~/.emacs.d/elpa-copy``
directory. There is already a several functions from autoload.el that will
help for that:

- ``(update-autoloads-from-directories &rest DIRS)`` which writes the output
  to the file identified by the ``generated-autoload-file`` variable.   It is
  therefore possible to let-bind this variable to some specific file that we
  could run.

  - The generated autoload could be called ``pel-generated-autoload.el`` and
    stored in some known location, then loaded by PEL when starting.
  - The process of creating the ``pel-generated-autoload.el`` will take some
    time.  It might be several seconds and will increase as the number of
    files inside ``~/.emacs.d/elpa-copy`` will grow.  So this must be done
    when new packages are installed, **not** when Emacs starts.

I ran the following snippet of code on the files located in my
``~/.emacs.d/elpa-copy`` directory:

.. code:: lisp

          (let ((generated-autoload-file "~/tmp/pel-generated-autoload.el"))
            (update-autoloads-from-directories "~/.emacs.d/elpa-copy"))

This generated the file ``~/tmp/pel-generated-autoload.el``.  That file has
18,353 lines.  The file remembers that the source code files are inside the
``elpa-copy`` directory, which is OK because this is where the code files are
located.

Unfortunately the content of the generated-autoload.el identifies the
directory as ../.emacs.d/elpa-copy.  It should be absolute.  So for the moment
I just replaced it manually, and I'll need to find out how to properly
generate it.


Quickly flip between the original elpa and a reduced elpa directory
-------------------------------------------------------------------

The Emacs package.el deals with the real, complete, Elpa-compliant directory,
which normally is ``~/.emacs.d/elpa``.

To speed things up it copied the source code files of all packages that have
no sub-directories into the ``elpa-copy`` directory.  Then I created another
directory, called ``~/.emacs.d/elpa-reduced`` which contains the other elpa
packages, the ones that have sub-directories.

For speedy startup, I want to use ``elpa-reduced`` and ``elpa-copy`` instead
of ``elpa`` to reduce the overall number of sub-directories and therefore the
number of entries inside the Emacs ``load-path``.

And I want to be able to quickly flip between the 2 sets: I'll use elpa when
interacting with the package.el commands (to allow the user to take advantage
of package.el package management facilities) and will switch elpa-reduced
otherwise.  I'll have to look into the details to see if that strategy will
work.

But for now, I just want to see if adding the loading of the autoload file
will slow this too much.

So what I can do is to is:

- rename ``~/.emacs.d/elpa`` to ``~/emacs.d/elpa-complete``
- create a ``~/.emacs.d/elpa`` symlink that points to either:

  - ``~/emacs.d/elpa-complete``, or
  - ``~/emacs.d/elpa-reduced``

Then I modify my init file to load the file
``~/tmp/pel-generated-autoload.el`` that I generated previously.

I try Emacs, and.... it works! I can use all features as if everything was in
the complete original elpa directory!

And it **still** starts fast!  Same 0.2 seconds as before!

Here's the ``make timeit`` report.  It's a little longer than before (most
probably because of the loading of the autoloads) but still fast::

    >Pierres-iMac@Fri Jul 02@22:07:52[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/tmp/pel-generated-autoload.el (source)...
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    -  27 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.15
    user         0.03
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.14
    user         0.03
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         0.78
    user         0.58
    sys          0.08
    >Pierres-iMac@Fri Jul 02@22:07:57[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/tmp/pel-generated-autoload.el (source)...
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    -  27 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         0.77
    user         0.58
    sys          0.08
    >Pierres-iMac@Fri Jul 02@22:16:23[~/dev/elisp/pel]
    > make timeit
    ***** Running Emacs startup time measurement tests
    ** Report Configuration settings.
    emacs --batch -L . -l "~/.emacs.d/init.el" -l pel-package.el -f pel-package-info
    Loading /Users/roup/tmp/pel-generated-autoload.el (source)...
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...
    Cleaning up the recentf list...
    Cleaning up the recentf list...done (0 removed)
    PEL loaded, PEL keys binding in effect.
    -  27 Elpa packages stored in : /Users/roup/.emacs.d/elpa/
    -  30 Utils files   stored in : /Users/roup/.emacs.d/utils/
    - size of load-path           : 36 directories
    - Number of PEL user-options  : 250 (198 are active)
    - PEL activated elpa  packages: 167 (  0 dependants, 5 imposed by restrictions)
    - PEL Activated utils files   :  25 (  0 dependants, 0 imposed by restrictions)

    ** Time measurement:
    time -p emacs -nw -Q -e kill-emacs
    real         0.13
    user         0.02
    sys          0.01
    time -p emacs -nw -q -e kill-emacs
    real         0.13
    user         0.03
    sys          0.01
    time -p emacs -nw -e kill-emacs
    real         0.75
    user         0.56
    sys          0.08
    >


The ``~/tmp/pel-generated-autoload.el`` has file variable that prevents byte
compilation of this file.  I don't see yet why that could not be
byte-compiled, so I'll have to do a little more research.  But perhaps it's
possible to byte compile it and reduce time a little bit more.  We'll see...


O.2 second Emacs startup with 192 external packages!
----------------------------------------------------

Elpa packages stored in their original directories (these are the ones that
have sub-directories) and 30 Emacs Lisp files stored in PEL Utils, I am
getting Emacs 26.3 to start in 0.2 seconds!!  And this is **without using
use-package** (even though PEL uses the same general techniques).

In the `use-package introduction`_, John Wiegley writes:

 "*I created it because I have over 80 packages that I use in Emacs, and things
 were getting difficult to manage. Yet with this utility my total load time is
 around 2 seconds, with no loss of functionality!*"

Now,  with *almost* no loss of Emacs functionality, but with loss of PEL's ability to
install software, I end up with a system that uses 192 external packages and
starts in 0.2 second.  That's about **24 times faster**! On Emacs 26.3!

Some problems remain:

Problem 1 : unresolved dependencies of elpa-reduced packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The packages that have multiple directories remained in the ``elpa-reduced``
directory and remain managed by package.el as before.  That is not a problem
in itself, however package.el fails to find the dependencies of those
packages inside the Emacs Lisp files copied into ``~/.emacs.d/elpa-copy``
files.

When Emacs starts it displays the following messages::

    Loading /Users/roup/tmp/pel-generated-autoload.el (source)...done
    Loading /Users/roup/.emacs.d/emacs-customization.el (source)...done
    Loading pel_keys...
    Loading /Users/roup/.emacs.d/recentf...done
    Cleaning up the recentf list...done (0 removed)
    Loading pel_keys...done
    PEL loaded, PEL keys binding in effect.
    Unable to activate package ‘yasnippet-snippets’.
    Required package ‘yasnippet-0.8.0’ is unavailable
    Unable to activate package ‘treemacs’.
    Required package ‘dash-2.11.0’ is unavailable
    Unable to activate package ‘relint’.
    Required package ‘xr-1.20’ is unavailable
    Unable to activate package ‘racket-mode’.
    Required package ‘faceup-0.0.2’ is unavailable
    Unable to activate package ‘origami’.
    Required package ‘s-1.9.0’ is unavailable
    Unable to activate package ‘lsp-ui’.
    Required package ‘dash-2.18.0’ is unavailable
    Unable to activate package ‘lsp-treemacs’.
    Required package ‘dash-2.18.0’ is unavailable
    Unable to activate package ‘geiser-racket’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘geiser-guile’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘geiser-gambit’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘geiser-chicken’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘geiser-chibi’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘geiser-chez’.
    Required package ‘geiser-0.16’ is unavailable
    Unable to activate package ‘clojure-snippets’.
    Required package ‘yasnippet-0.10.0’ is unavailable
    Unable to activate package ‘auto-complete’.
    Required package ‘popup-0.5.0’ is unavailable
    Unable to activate package ‘alchemist’.
    Required package ‘elixir-mode-2.2.5’ is unavailable
    Starting new Ispell process aspell with default dictionary...
    Loading pel__hydra...done
    Emacs startup time: 0.2 seconds
    Updating buffer list...
    Formats have changed, recompiling...done
    Updating buffer list...done
    Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help
    Mark set
    Updating buffer list...done
    Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help



The files for the reported missing code **is** located inside some of the
directories that remained inside the ``~/.emacs.d/elpa-reduced`` directories
but for some reason have not been integrated inside the Emacs load-path::

   1 /Users/roup/.emacs.d/elpa/company-20210618.2105
   2 /Users/roup/.emacs.d/elpa/forth-mode-20210123.900
   3 /Users/roup/.emacs.d/elpa/lice-20200607.103
   4 /Users/roup/.emacs.d/elpa/monky-20201226.1950
   5 /Users/roup/.emacs.d/elpa/neotree-20200324.1946
   6 /Users/roup/.emacs.d/elpa/package-lint-20210326.241
   7 /Users/roup/.emacs.d/elpa/seq-2.22
   8 /Users/roup/.emacs.d/elpa/sly-20210303.1148
   9 /Users/roup/.emacs.d/elpa/vterm-20210313.1359
  10 /Users/roup/dev/elisp/pel
  11 /Users/roup/.emacs.d/utils
  12 /Users/roup/.emacs.d/elpa-copy
  13 /usr/local/share/emacs/site-lisp
  14 /usr/local/share/emacs/site-lisp/autoconf
  15 /usr/local/share/emacs/site-lisp/clisp
  16 /usr/local/share/emacs/site-lisp/cmake
  17 /usr/local/share/emacs/site-lisp/gerbil-scheme
  18 /usr/local/share/emacs/site-lisp/gettext
  19 /usr/local/share/emacs/site-lisp/git
  20 /usr/local/share/emacs/site-lisp/lfe
  21 /usr/local/share/emacs/site-lisp/libidn
  22 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp
  23 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/vc
  24 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/url
  25 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/textmodes
  26 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/progmodes
  27 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/play
  28 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/org
  29 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/nxml
  30 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/net
  31 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/mh-e
  32 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/mail
  33 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/leim
  34 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/language
  35 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/international
  36 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/image
  37 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/gnus
  38 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/eshell
  39 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/erc
  40 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/emulation
  41 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/emacs-lisp
  42 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/cedet
  43 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/calendar
  44 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/calc
  45 /usr/local/Cellar/emacs/26.3/share/emacs/26.3/lisp/obsolete



The list of directories inside elpa-reduced is::

    alchemist-20180312.1304
    archives
    auto-complete-20201213.1255
    clojure-snippets-20180314.1308
    company-20210618.2105
    forth-mode-20210123.900
    geiser-chez-20210421.120
    geiser-chibi-20210421.123
    geiser-chicken-20210421.127
    geiser-gambit-20210421.124
    geiser-guile-20210508.1838
    geiser-racket-20210421.125
    gnupg
    lice-20200607.103
    lsp-treemacs-20210502.1804
    lsp-ui-20210604.1158
    monky-20201226.1950
    neotree-20200324.1946
    origami-1.0
    package-lint-20210326.241
    racket-mode-20210629.2000
    relint-1.19
    seq-2.22
    sly-20210303.1148
    treemacs-20210606.1918
    vterm-20210313.1359
    yasnippet-snippets-20210105.1346

The exact dependencies are not there but a development version for the
following missing requirement is

And If I use Emacs 27 or later, use gccemacs and use Emacs in daemon mode
I'll benefit for these speedup as well.

Notice also that for some reason I don't yet understand, using symlinks did
not work.  That needs to be investigated.








What's Next?
============

I need to learn package.el and Emacs startup mechanism to understand why it's
much faster to use a smaller number of elpa sub-directories, why symlinks did
not fail and how I can make PEL be able to support automatic detection and
installation while starting real fast as it did in the last step of the
experiment.   I also need to understand how to handle Emacs Lisp packages that
have sub-directories and see if I can find a way to put all files inside a
single directory that ideally would contain symlinks to the real location of
the Emacs Lisp files.  This way I could design something that uses an existing
package manager like package.el or also perhaps something like Borg or
straight and provide a layer on top in a form of a single directory with
symlinks to everything.  And while I'm at it why not also do this for all
files in Utils and for the native Emacs Lisp packages and end up with **only
one** directory in my load-path.  That should speed things up even more.

I always wondered why we use a large number of directories in the Emacs
load-path. I understand that its good to keep un-related files inside their
own, separate, directories.  That's a requirement for several things,
including DVCS like Git.  But why not use a *small* number of directories in
the load-path which contain symlinks to the location to real files?  We do
that on Unix-like OS all the time.

On the system where this was tested, the following directories are in PATH,
The number before identifies the number of files.  Most of them are symlinks::

 1518 files: /usr/local/bin
  969 files: /usr/bin
   35 files: /bin
  235 files: /usr/sbin
   62 files: /sbin
  127 files: /opt/X11/bin
    7 files: /usr/local/sbin

That is 2953 files.  Could you imagine having 1000 entries in your PATH?  Or
even 100?

Having one directory per OS-level utility package identified on the system or
a shell process PATH would rightly be considered insane. What people do is use
symlinks and a small number of entries in the PATH.
So why not use the same technique inside Emacs?


My next step will be to investigate this idea and ideally come up with code
that automatically handle the auto-loading and
integrates with PEL but with anything else, perhaps an independent
package that anybody would be able to use.  Hopefully, that will be possible.



.. ---------------------------------------------------------------------------

.. _gccemacs: http://akrl.sdf.org/gccemacs.html
.. _use-package introduction: https://github.com/jwiegley/use-package#readme


.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

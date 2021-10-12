===============================
Developing Erlang Code with PEL
===============================

:Home URL: https://github.com/pierre-rouleau/pel/blob/master/doc/developing-erlang.rst
:Project: `PEL Project home page`_
:Created:  Thursday, June  3 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2021-10-12 16:43:11, updated by Pierre Rouleau.
:Copyright: © 2021, Pierre Rouleau


This document describes PEL facilities that help developing software written
in Erlang.

🚧 This is an early version of a work-in-progress document while I complete
Erlang support in PEL.  My goal is to complete this document sometime in
fall of 2021.  I need to deepen  my understanding of the erlang.el library along
with the various external packages that support Erlang.  If you read this and
have ideas as to what you would like to see don't hesitate to enter a message
in form or an issue_ or in the discussions_.


.. contents::  **Table of Contents**
.. sectnum::

.. ---------------------------------------------------------------------------

Goals
=====

- Easy to setup.
- Fully documented.
- Flexible.
- Support several versions of Erlang, with the ability to run multiple OS
  shells, each one specialized with a specific version of Erlang and running
  Emacs with PEL that provides:

  - access to the local man pages for the version of Erlang available in the
    parent OS shell,
  - ability to run various Erlang tools, including the Erlang shell, from
    within Emacs, with specialized configurations.
  - ability to run various tests on the code and:

    - open the generated HTML documentation pages,
    - see test results in text and infer test success or failure.

- Support for extended and specialized code templates to ease file creation
  with:

  - automatic and customizable identification of author, created date,
    timestamp, copyright notice and open source code license.

- Support for IDE-like features made available by LSP-based back-end language
  server.
- Ability to use all of this in Emacs running in graphical mode or in terminal
  (termcap) mode.

.. ---------------------------------------------------------------------------

Installing Erlang
=================

Instructions on how to install Erlang in various ways and how to install
Erlang man files is available in the `Installing Erlang`_ document.  This
document includes the following sections:

- Installing Erlang:

  - `Installing Erlang using Homebrew`_
  - `Using Erlang Installer from Erlang Solutions`_
  - `Using Kerl to build Erlang from source code`_
  - `Using asdf-vm to build Erlang from source code`_

- Further installation steps:

  - `Manual installation of Erlang OTP Documentation and Man Files`_
  - `Creating whatis files for Erlang man pages`_
  - `Using Erlang Man files within Emacs`_
  - `Using Specialized OS Shells for Erlang`_
  - `Using PEL with Specialized Shells for Erlang to Edit Erlang`_
  - `The ~/.erlang startup file`_

.. ---------------------------------------------------------------------------


.. _issue:                                                         https://github.com/pierre-rouleau/pel/issues
.. _discussions:                                                   https://github.com/pierre-rouleau/pel/discussions
.. _Installing Erlang:                                             https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang.rst
.. _PEL Project home page:                                         https://github.com/pierre-rouleau/pel#readme
.. _Installing Erlang using Homebrew:                              https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang-hb.rst
.. _Using Erlang Installer from Erlang Solutions:                  https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang-ei.rst
.. _Using Kerl to build Erlang from source code:                   https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang-kerl.rst
.. _Using asdf-vm to build Erlang from source code:                https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang-asdf.rst
.. _Manual installation of Erlang OTP Documentation and Man Files: https://github.com/pierre-rouleau/about-erlang/blob/master/doc/installing-erlang-man-files.rst
.. _Creating whatis files for Erlang man pages:                    https://github.com/pierre-rouleau/about-erlang/blob/master/doc/whatis-files.rst
.. _Using Erlang Man files within Emacs:                           https://github.com/pierre-rouleau/about-erlang/blob/master/doc/erlang-man-with-emacs.rst
.. _Using Specialized OS Shells for Erlang:                        https://github.com/pierre-rouleau/about-erlang/blob/master/doc/specialized-shells.rst
.. _Using PEL with Specialized Shells for Erlang to Edit Erlang:   https://github.com/pierre-rouleau/about-erlang/blob/master/doc/editing-erlang-with-pel.rst
.. _The ~/.erlang startup file:                                    https://github.com/pierre-rouleau/about-erlang/blob/master/doc/file-erlang-startup.rst




.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

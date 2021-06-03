==========================
Developing Erlang with PEL
==========================

:Home URL: https://github.com/pierre-rouleau/pel/blob/master/doc/developing-erlang.rst
:Project: `PEL Project home page`_
:Created:  Thursday, June  3 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2021-06-03 10:29:42, by Pierre Rouleau.
:Copyright: © 2021, Pierre Rouleau


This document describes PEL facilities that help developing software written
in Erlang.

🚧 This is an early version of a work-in-progress document while I complete
Erlang support in PEL.  My goal is to complete this document sometime in
June 2021.  I need to deepen  my understanding of the erlang.el library along
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


.. _issue: https://github.com/pierre-rouleau/pel/issues
.. _discussions: https://github.com/pierre-rouleau/pel/discussions




.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

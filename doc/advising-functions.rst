================================
Advising Functions in Emacs Lisp
================================

:Home URL:
:Project:
:Created:  Sunday, July 18 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2021-07-18 12:34:56, by Pierre Rouleau.
:Copyright: © 2021, Pierre Rouleau


.. contents::  **Table of Contents**
.. sectnum::

.. ---------------------------------------------------------------------------



Run Code After Execution of a Function
--------------------------------------

Use the following code to add code that runs after the base command function
``foo``:

.. code:: elisp

          (defun foo ()
            "Say hello."
            (interactive)
            (insert "hello"))

          (defadvice foo (after foo-after activate)
            (insert " world!\n"))

After that the ``foo`` command inserts "hello world!\n".

Run Code Before Execution of a Function
---------------------------------------

The following defadvice adds code that inserts the string "And you say: "
before what ``foo`` inserts:

.. code:: elisp

          (defadvice foo (before foo-before activate)
            (insert "And you say: "))


Override a Function
-------------------

Use the following form to override function:

.. code:: elisp

          (advice-add 'original-function :override #'replacement-function)


To restore the original function use the advice-remove:

.. code:: elisp

          (advice-remove 'original-function #'replacement-function)



.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

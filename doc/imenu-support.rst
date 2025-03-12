=========================
iMenu Support Description
=========================

:Home URL: https://github.com/pierre-rouleau/pel
:Project: `PEL -- Pragmatic Emacs Library`_
:Created:  Wednesday, May 12, 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2025-03-11 23:59:19 EDT, updated by Pierre Rouleau.
:Copyright: © 2021, 2025, Pierre Rouleau


Introduction
============

imenu support for a major mode provides useful functionality that is normally
expected from several major modes including all major modes that derive from
the ``prog-mode``.

Also see the `How to add iMenu and Speedbar support for a major mode PDF`_.

.. _How to add iMenu and Speedbar support for a major mode PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/imenu-speedbar-support.pdf


Support for Major Modes
=======================


Emacs Lisp
----------

imenu-generic-expression
^^^^^^^^^^^^^^^^^^^^^^^^

::

  ((nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
   (#1="Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
   (#1# "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]
  ]+[^)]" 1)
   ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))


Python
------

The imenu variables are not bound at first: they are all nil.

However, the variable ``py-imenu-generic-expression`` is set by the Emacs distributed
copy of python-mode.el:

Its value is:
  ("\\(^[ 	]*\\(class[ 	]+[a-zA-Z0-9_]+\\)\\([ 	]*\\((\\([a-zA-Z0-9_,.
  ]\\)*)\\)?\\)[ 	]*:\\)\\|\\(^[ 	]*\\(def[ 	]+\\([a-zA-Z0-9_]+\\)[ 	]*(\\(.*\\))\\)[ 	]*:\\)" 2 8)

It is set like this::

  (defvar py-imenu-generic-expression
    (cons
     (concat
      py-imenu-class-regexp
      "\\|"                               ; or...
      py-imenu-method-regexp
      )
     py-imenu-method-no-arg-parens)

where::

  (defvar py-imenu-class-regexp
    (concat                               ; <<classes>>
     "\\("                                ;
     "^[ \t]*"                            ; newline and maybe whitespace
     "\\(class[ \t]+[a-zA-Z0-9_]+\\)"     ; class name
                                          ; possibly multiple superclasses
     "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
     "[ \t]*:"                            ; and the final :
     "\\)"                                ; >>classes<<
     )
    "Regexp for Python classes for use with the Imenu package."
    )

  (defvar py-imenu-method-regexp
    (concat                               ; <<methods and functions>>
     "\\("                                ;
     "^[ \t]*"                            ; new line and maybe whitespace
     "\\(def[ \t]+"                       ; function definitions start with def
     "\\([a-zA-Z0-9_]+\\)"                ;   name is here
                                          ;   function arguments...
     ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
     "[ \t]*(\\(.*\\))"
     "\\)"                                ; end of def
     "[ \t]*:"                            ; and then the :
     "\\)"                                ; >>methods and functions<<
     )
    "Regexp for Python methods/functions for use with the Imenu package.")


.. ---------------------------------------------------------------------------

Go
--


Content of imenu-generic-expression::

    1 - ("type" "^type *\\([^
  ]*\\)" 1)
    2 - ("func" "^func *\\(.*\\) {" 1)


- Content of imenu-case-fold-search: nil
- Content of imenu-syntax-alist: nil


SQL
---

From: https://developpaper.com/exploiting-emacs-imenus-potential/

.. code:: lisp

    (setq sql-imenu-generic-expression
           '(("Comments" "^-- \(.+\)" 1)
         ("Function Definitions" "^\s-*\(function\|procedure\)[ \n\t]+\([a-z0-9_]+\)\
     [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\(return[ \n\t]+[a-z0-9_]+[ \n\t]+\)?[ai]s\b" 2)
         ("Function Prototypes" "^\s-*\(function\|procedure\)[ \n\t]+\([a-z0-9_]+\)\
     [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\(return[ \n\t]+[a-z0-9_]+[ \n\t]*\)?;" 2)
         ("Indexes" "^\s-*create\s-+index\s-+\(\w+\)" 1)
         ("Tables" "^\s-*create\s-+table\s-+\(\w+\)" 1)))

    (add-hook 'sql-mode-hook
            (lambda ()
               (setq imenu-generic-expression sql-imenu-generic-expression)))


.. ---------------------------------------------------------------------------
.. links

.. _PEL -- Pragmatic Emacs Library: https://github.com/pierre-rouleau/pel#readme

.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

=========================
iMenu Support Description
=========================


Introduction
============


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

The imenu variables are not bound at first: the are all nil.

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
  
    2 - ("func" "^func *\\(.*\\) {" 1)


- Content of imenu-case-fold-search: nil
- Content of imenu-syntax-alist: nil

.. ---------------------------------------------------------------------------
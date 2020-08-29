;;; custom-c-skel.el --- Custom skeleton example.  -*- lexical-binding: t; -*-

;; Created   : Friday, August 28 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-08-29 17:46:13, updated by Pierre Rouleau>

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This is an example of a custom PEL compatible tempo skeleton that defines C
;; code features.  Use this as a template for you own custom skeletons, using
;; the same function names.  Store your file somewhere else and identify the
;; name of this file inside the corresponding PEL user option.
;;
;; defun name                               PEL user option
;; ===================================      =================================
;; `pel-skels-c-function-def/custom'        `pel-c-skel-function-define-style'
;;
;;
;; This uses the same facilities than the code inside pel-c-skels.  The code
;; here can require any PEL .el file except pel_keys.el.  It can also require
;; Emacs libraries.  You have the full power of Emacs tempo skeletons and Emacs
;; Lisp.

;; Note: if you code depends on functions inside pel-skels-c.el or what it uses
;;       you won't need to write require forms to load them.  This is the case
;;       in the example below.  You will need to add them if you get an error
;;       when attempting to insert a function definition with your code. Put
;;       those statements inside the Dependencies section below or use lazy
;;       require techniques written inside functions in a way similar to how PEL
;;       code is written (look for require forms that pass the :noerror argument).

;; The example below create code that looks like this:
;;
;;      /* -------------------------------------------------------------------------- */
;;      /* =========================
;;       * list_registered_processes
;;       * =========================
;;       *
;;       * Print or display the list of registered process on the specified device.
;;       *
;;       */
;;
;;      void
;;      list_registered_processes()
;;      {
;;
;;      }


;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; ----------------------------------------------------------------------------
;;; Code:
;;

(defun pel-skels-c-header-module-block/custom (fname is-a-header cmt-style)
  "Example of a custom skeleton for the top of a C file.
The arguments are:
- FNAME := string.  the name of the current file without path.
- IS-A-HEADER := boolean. non-nil if the file is a C header file, nil otherwise.
- CMT-STYLE := a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string."
  (let* ((purpose  (pel-prompt-purpose-for "File" 'p))
         (cb       (nth 0 cmt-style))
         (cc       (nth 1 cmt-style))
         (ce       (nth 2 cmt-style)))
    (list
     'l
     cb " " fname " : " purpose  'n
     cc 'n
     cc (format-time-string
      " U-FooBar restricted an and confidential. Copyright © U-FooBar %Y.\n")
     (pel-skel-created-comment cc)
     (pel-skel-author-comment  cc)
     (pel-skel-time-stamp pel-c-skel-insert-file-timestamp cc)
     ce (pel-when-text-in ce 'n)
     (pel-separator-line) 'n)))


(defun pel-skels-c-function-def/custom ()
  "Example of a custom skeleton function that defines a C function definition."
  ;; this let form creates and initializes the local variables
  (let* ((fct-name        (pel-prompt-function (function pel-valid-c-function-name)))
         (purpose         (pel-prompt-purpose-for "Function" 'p))
         (c-style         (pel-c-style-comments-strings))
         (cb              (nth 0 c-style))   ; comment begin: "/*" or "//"
         (cc              (nth 1 c-style))   ; comment continue: "**", " *" or "//"
         (ce              (nth 2 c-style))  ; comment end: "*/", " */",  or ""
         (line            (make-string (length fct-name) ?=)))
    ;; create and return a tempo skeleton inclusion list that creates the C code
    (list
     'l
     (pel-skel-c-separator-line)   ; insert a separator line if required
     cb " " line 'n
     cc " " fct-name 'n
     cc " " line 'n
     cc 'n
     cc " " purpose 'n
     cc 'n
     ce 'n (pel-when-text-in ce 'n)
     (pel-skels-c-function-def fct-name))))

;;; ----------------------------------------------------------------------------
(provide 'custom-c-skel)

;;; custom-c-skel.el ends here

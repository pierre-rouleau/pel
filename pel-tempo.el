;;; pel-tempo.el --- Specialized tempo mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; -----------------------------------------------------------------------------
;;; Attribution:
;;
;; The ideas that triggered writing this code came from version 2.4.8 of the
;; erlang.el and its associate erlang-skel.el file, available at GitHub at
;; URL https://github.com/erlang/otp/blob/master/lib/tools/emacs/erlang.el
;;
;; The functions `pel-tempo-create' and `pel-tempo-include' draw from ideas
;; in code written in erlang.el and erlang-skel.el; generalizing the ideas
;; to allow support of other programming languages while retaining the
;; ability to be used to support Erlang.

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines logic to create interactive functions that insert text
;; generated with the tempo skeleton mechanism.
;;
;; It also defines the very simple pel-tempo-mode, a mode that does only one
;; thing: it activates a key-map to ease execution of the
;; `tempo-forward-mark' and `tempo-backward-mark' commands.  These commands
;; move point to the next or previous template "hot-spot" (officially called
;; tempo marks): the location where extra text of the template must be
;; filled in by the user.
;;
;; While active the pel-tempo-mode displays its short lighter: " ‡".
;;
;;
;; - PEL Tempo minor mode:
;;
;;   - `pel-tempo-mode'
;;     > `pel-tempo-minor-mode-map'
;;
;; - Tempo template creation utilities:
;;
;;   A set of functions used to simplify the creation of commands that insert
;;   tempo skeletons along with the ability to create a menu of those skeletons.
;;
;;   - Create tempo-template interactive functions:
;;
;;     - `pel-tempo-create'
;;       - `pel-tempo-include'
;;
;;   - Create the PEL tempo-template interactive functions with bindings
;;
;;     - `pel-tempo-install-pel-skel'
;;

;; -----------------------------------------------------------------------------
;;; Code:

;; pel-tempo-mode
;; --------------
;;
;; Defines the pel-tempo-mode, a minor mode that provides access to key bindings
;; to navigate inside the tempo template.

(defvar pel-tempo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use the standard go next/previous keys
    (define-key map (kbd "C-c M-f") 'tempo-forward-mark)
    (define-key map "\C-c."         'tempo-forward-mark)
    (define-key map (kbd "C-c M-b") 'tempo-backward-mark)
    (define-key map "\C-c,"         'tempo-backward-mark)
    (when (display-graphic-p)
      (define-key map (kbd "C-c C-.") 'tempo-forward-mark)
      (define-key map (kbd "C-c C-,") 'tempo-backward-mark))
    map)
  "Key-map for pel-tempo minor mode.")

(define-minor-mode pel-tempo-mode
  "Minor mode to ease navigation in tempo template hot-spots.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When pel-tempo-mode is on, extra, easy-to-use key bindings are
bound to the `tempo-forward-mark' and `tempo-backward-mark'
commands to help navigate to the next and previous tempo mark.

Key1 bindings:
\\{pel-tempo-minor-mode-map}"
  :lighter " ‡"
  :keymap pel-tempo-minor-mode-map)

;; -----------------------------------------------------------------------------
;; Tempo template creation utilities
;; =================================

;; --
;; Tempo-compliant skeleton inclusion

(defun pel-tempo-include (&rest args)
  "Return a tempo include list of the elements in ARGS.
The first element of the returned list is the 'l symbol and then
each other elements are the elements of each ARG, where ARG is one of the ARGS
and is itself a list.

This is used to inserts several elements inside a tempo template."
  (let (elements)
    (dolist (arg args)
      (dolist (elem arg)
        (setq elements (cons elem elements))))
    (cons 'l (nreverse elements))))

(defun pel-tempo-include-when (option &rest forms)
  "Include a tempo template inside another one.
Used like this:
  (defvar fct-skel '(\"%%% Important function:\"
                     (pel-tempo-include-when option
                                             pel-skel-erlang-func)))

This returns what tempo expects: a list form with the symbol
'l as the first element and the FORMS specified expanded.
if OPTION is non-nil otherwise it return nil."
  (when option
    (let (result)
      (dolist (form forms)
        (dolist (elem form)
          (setq result (cons elem result))))
      (cons 'l (nreverse result)))))

;; --

(defun pel-tempo-create (mode skeletons  &optional menu-item-creator-function)
  "Create commands that insert MODE code templates using provided SKELETONS.
- MODE: string: the name of a programming language or markup language
        for which the SKELETONS are provided.
        The name of created interactive function is 'tempo-template-MODE-NAME'
        where MODE is taken from the argument and NAME is taken from the second
        element of the SKELETONS list entry.
- SKELETONS: a list of skel-elem: (menu-tag: string, name: string, code: list)
         Each entry defines one skeleton.  The menu-tag is the text to display
         inside the menu. The name is used in the function name. The code is the
         tempo skeleton elements.  The can be other elements inside the
         skel-elem list, these will be ignored but are passed to the
         MENU-ITEM-CREATOR-FUNCTION.
- MENU-ITEM-CREATOR-FUNCTION: symbol (or nil).  The function used it to create a
         skeleton menu entry. Takes one element: the skel-elem list.
         If MENU-ITEM-CREATOR-FUNCTION is nil, no menu is created.
Return the created skeleton menu list (or nil if no MENU-ITEM-CREATOR-FUNCTION)."
  (if (and (require 'tempo nil :noerror)
           (fboundp 'tempo-define-template))
      (let (menu
            (menu-create (or menu-item-creator-function 'ignore)))
        (dolist (skel-elem skeletons)
          (let ((menu-tag  (car skel-elem))
                (name      (nth 1 skel-elem))
                (skel-code (nth 2 skel-elem)))
            (if (null menu-tag)
                ;; menu separator
                (setq menu (cons nil menu))
              ;; skeleton entry
              ;; - call tempo-define-template with its arguments
              (funcall 'tempo-define-template
                       (concat mode "-" name)
                       (list (list 'pel-tempo-include skel-code))
                       name)
              ;; - insert a menu item for the skeleton
              (setq menu (cons (funcall menu-create skel-elem)
                               menu)))))
        ;; return accumulated menu - nil if none
        (if menu-item-creator-function menu nil))
    (error "Failed loading tempo!")))



;; Install already created tempo skeleton functions to specified bindings.
;; -----------------------------------------------------------------------
;;

(defun pel-tempo-install-pel-skel (mode skeletons key-map keys-alist
  &optional mode-abbrev use-existing-tempo-function)
  "Create commands to insert MODE specific SKELETONS functions.
These commands are bound to keys defined in the
KEYS-ALIST, a alist of (skel-name . key) bound inside the KEY-MAP.
The skel-name must correspond to one of the names in the SKELETONS: the
  second element of a SKELETONS entry.
The commands created have names that are like 'pel-ABBREV-NAME' where
ABBREV is MODE-ABBREV if specified (or MODE otherwise), and NAME corresponds
to the second element of the SKELETONS entry.
Creates the tempo skeleton function with `tempo-define-template' unless
USE-EXISTING-TEMPO-FUNCTION is non-nil, in which case it assumes it is already
defined.
The function also binds the functions `tempo-complete-tag` and
the function `pel-tempo-mode` to <f12> and <space> respectively
inside the specified KEY_MAP."
  ;; bind non skeleton keys in the key map
  (define-key key-map (kbd "<f12>") 'tempo-complete-tag)
  (define-key key-map " " 'pel-tempo-mode)
  (setq mode-abbrev (or mode-abbrev mode))
  (let ((cap-mode     (capitalize mode)))
    (dolist (skel skeletons)
      (when skel
        (let ((s-name  (nth 1 skel)))
          (unless use-existing-tempo-function
            (if (and
                 (require 'tempo nil :noerror)
                 (fboundp 'tempo-define-template))
                ;; create the tempo-template-NAME file where NAME
                ;; is made of mode and skeleton name
                (tempo-define-template
                 (format "%s-%s" mode s-name)
                 (list (list 'pel-tempo-include (nth 2 skel))))
              (user-error "Failed loading tempo!")))
          (let* ((s-tempo-fname (format "tempo-template-%s-%s"
                                        mode s-name))
                 (s-tempo-fun   (intern s-tempo-fname))
                 (s-pel-fname   (format "pel-%s-%s" mode-abbrev s-name))
                 (s-docstring   (format "\
Insert '%s' %s skeleton's text (also available through %s/Skeleton menu).

This function is dynamically defined by a call to the function \
`pel-tempo-install-pel-skel'."
                                        s-name cap-mode cap-mode))
                 (assoc-value   (cdr (assoc s-name keys-alist)))
                 (key           (if (listp assoc-value)
                                    (car assoc-value)
                                  assoc-value))
                 (prep-func     (when (listp assoc-value)
                                  (car (cdr assoc-value)))))
          ;; Define the PEL command that inserts the text
          ;; for the specific template. It uses the tempo function
          ;; and activates the pel-tempo-mode minor mode.
          ;; If a preparation function is required, the command
          ;; calls it, passing the tempo function instead of calling
          ;; the tempo function directly.
          (if prep-func
              (defalias (intern s-pel-fname)
                (lambda ()
                  (interactive)
                  (funcall  prep-func s-tempo-fun)
                  (pel-tempo-mode 1))
                s-docstring)
            (defalias (intern s-pel-fname)
              (lambda ()
                (interactive)
                (funcall s-tempo-fun)
                (pel-tempo-mode 1))
              s-docstring))
          ;; Bind the function to the identified keystroke.
          (when key
            (define-key
              key-map
              (if (> (length key) 1) (kbd key) key)
              (intern s-pel-fname)))))))))

;; -----------------------------------------------------------------------------
(provide 'pel-tempo)

;;; pel-tempo.el ends here

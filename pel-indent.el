;;; pel-indent.el --- PEL indentation utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-27 17:00:34 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2023, 2024, 2025  Pierre Rouleau
;;
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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file provides rigid indentation support.  It is meant to be used in
;; files where the super useful automatic indentation of the function
;; `indent-for-tab-command' is used (and assigned to the tab key).
;;
;; The force rigid indentation is needed in several scenarios.  It's not that
;; useful for edit Lisp source code but is useful for other modes such as C
;; and other curly bracket programming languages as well as indentation
;; sensitive programming languages like Python.
;;
;; 3 commands are provided:
;;
;; - Function `pel-indent-lines' and `pel-unindent-lines' rigidly indent and
;;   un-indent the region by a specified number of indentation levels which
;;   defaults to 1.
;; - Function `pel-indent-rigidly' allows interactive indentation by 1
;;   character at a time or indentation level at a time.
;;
;; The number of columns used for the indentation level used by the functions
;; `pel-indent-lines' and 'pel-unindent-lines' is returned by the function
;; `pel-indent-level-colums'.
;;
;; The function `pel-indent-lines' and `pel-unindent-lines' handle hard tabs
;; properly according to the currently active `indent-tabs-mode'.
;;
;; - When hard tabs are not permitted (i.e. `indent-tabs-mode' is nil), the
;;   function replace all hard tabs in the indentation by the appropriate
;;   number of space characters.  The functions do not replace hard tabs that
;;   are somewhere else on the line (e.g. inside a code string).
;; - When hard tabs are permitted (i.e. `indent-tabs-mode' is t), the
;;   functions tabify the line or region marked.

;; TODO: enhance pel-indent-level-columns to better consider various types of
;; files for various programming and markup languages.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)
;;; --------------------------------------------------------------------------
;;; Code:
;; ---------------------------------------------------------------------------


(defun pel-indent-current-line-positions ()
  "Return a cons with the positions of the beginning and end of current line."
  (let (begin end)
    (save-excursion
      (setq begin (progn
                    (beginning-of-line)
                    (point)))
      (setq end   (progn
                    (end-of-line)
                    (point))))
    (cons begin end)))

(defun pel-indent-tabify-current-line ()
  "Tabify the current line."
    (let ((begin.end (pel-indent-current-line-positions)))
      (tabify (car begin.end) (cdr begin.end))))

(defun pel-indent-untabify-current-line ()
  "Untabify the current line."
    (let ((begin.end (pel-indent-current-line-positions)))
      (untabify (car begin.end) (cdr begin.end))))

;; --

(defun pel-indent-hard-tab-in-region-or-line-p ()
  "Check if any lines is indented with a hard-tab in the marked region.
Return non-nil if there is at least one.  The returned value is the position
of the first hard tab found.
Return nil if there are no hard tab in the indentation."
  (let (begin-pos end-pos)
    (if (use-region-p)
        (progn
          (setq begin-pos (region-beginning))
          (setq end-pos   (region-end)))
      (let ((begin.end (pel-indent-current-line-positions)))
        (setq begin-pos (car begin.end))
        (setq end-pos   (cdr begin.end))))
    (save-excursion
      (goto-char begin-pos)
      (condition-case nil
          (re-search-forward "^ *\t" end-pos)
        (search-failed nil)))))

(defun pel-indent-untabify-region (marked-lines-spec)
  "Replace all hard tabs by spaces in the MARKED-LINES-SPEC region of lines.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (let ((first-line (nth 0 marked-lines-spec))
        (end-line   (nth 1 marked-lines-spec)))
    (untabify (pel-indent-line-pos first-line nil)
              (pel-indent-line-pos end-line t))))

(defun pel-indent-tabify-region (marked-lines-spec)
  "Use hard tabs and spaces in the MARKED-LINES-SPEC region of lines.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (let ((first-line (nth 0 marked-lines-spec))
        (end-line   (nth 1 marked-lines-spec)))
    (tabify (pel-indent-line-pos first-line nil)
            (pel-indent-line-pos end-line t))))

;; --
(defun pel-indent-level-columns (&optional n)
  "Return the number of columns corresponding to 1 (or N) indentation levels.
The indentation level depends on the type of buffer."
  (let ((cols-by-indentation-level   (if (and (boundp 'c-basic-offset)
                                              (integerp c-basic-offset))
                                         c-basic-offset
                                       tab-width)))
    (* (or n 1) cols-by-indentation-level)))

(defun pel-indent-marked-lines ()
  "Return information about currently marked lines.
The returned value is a list of 3 elements:
- line number of the first line in the region
- line number of the last line in the region
- symbol \\='point-before-mark or \\='mark-before-point."
  (list
   (line-number-at-pos (region-beginning))
   (line-number-at-pos (region-end))
   (if (< (point) (mark))
       'point-before-mark
     'mark-before-point)))

(defun pel-indent-line-pos (line at-end)
  "Return position of specified LINE at its first character unless AT-END."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (if at-end
        (move-end-of-line nil)
      (move-beginning-of-line nil))
    (point)))

(defun pel-indent-mark-lines (first-line end-line order)
  "Mark a region of lines identified by the arguments.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (cond ((eq order 'point-before-mark)
         (goto-char (pel-indent-line-pos first-line nil))
         (set-mark  (pel-indent-line-pos end-line t)))
        ((eq order 'mark-before-point)
         (set-mark  (pel-indent-line-pos first-line nil))
         (goto-char (pel-indent-line-pos end-line t)))
        (t
         (error "Invalid order argument value: %s" order))))


(defun pel-indent-mark-lines-by-spec (marked-lines-spec)
  "Mark a region of lines identified by the MARKED-LINES-SPEC argument.
The MARKED-LINES-SPEC argument is a list with the following 3 elements:
- line number of the first line in the region
- line number of the last line in the region
- symbol \\='point-before-mark or \\='mark-before-point."
  (pel-indent-mark-lines (nth 0 marked-lines-spec)
                         (nth 1 marked-lines-spec)
                         (nth 2 marked-lines-spec)))

(defun pel--insert-c-indent-line (n)
  "Insert N times the indentation level number of space chars on current line."
    (if (< n 0)
        (pel-unindent-lines (abs n))
      (move-beginning-of-line nil)
      (insert (make-string (pel-indent-level-columns n) ?\s))))


;;-pel-autoload
(defun pel-indent-lines (&optional n)
  "Indent current or marked lines by N indentation levels.
Works with point anywhere on the line.
All lines touched by the region are indented.
A special argument N can specify more than one
indentation level.  It defaults to 1.
If a negative number is specified, `pel-unindent-lines' is used.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines.
Handles presence of hard tabs:
- If `indent-tabs-mode' is non-nil the indentation is created with a mix
  of hard-tabs and space characters.
- If `indent-tabs-mode' is nil, any hard tab in the indentation of the
  marked lines is replaced by the proper number of spaces.
  Hard tabs after first non-whitespace character on the line are left."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (let ((has-hard-tab (pel-indent-hard-tab-in-region-or-line-p)))
      (if (use-region-p)
          (let ((original-region-spec (pel-indent-marked-lines))
                (begin-point (region-beginning))
                (line-count (count-lines (region-beginning) (region-end)))
                ;; Normally, Emacs deactivates the region right after a
                ;; command has executed.  To prevent this, and allow region to
                ;; stay active and visible for further execution of this
                ;; command, set the deactivate-mark variable to nil for this
                ;; form.
                (deactivate-mark nil))
            (when has-hard-tab
              (pel-indent-untabify-region original-region-spec))
            (deactivate-mark)
            (goto-char begin-point)
            (dotimes (_i line-count)
              (pel--insert-c-indent-line n)
              (forward-line 1))
            (when indent-tabs-mode
              (pel-indent-tabify-region original-region-spec))
            (pel-indent-mark-lines-by-spec original-region-spec))
        ;; handle single line
        (when has-hard-tab
          (pel-indent-untabify-current-line))
        (pel--insert-c-indent-line n)
        (when indent-tabs-mode
          (pel-indent-tabify-current-line))))))

;; --
(defun pel--line-unindent (&optional n)
  "Un-indent current line by N indentation levels.
Works when point is anywhere on the line.
Limitation: does not handle hard tabs and may move point."
  (back-to-indentation)
  (let ((n (prefix-numeric-value n)))
    (when (> (current-column) 0)
      (left-char (min (current-column) (pel-indent-level-columns n)))
      (if (and (require 'pel-ccp nil :no-error)
               (fboundp 'pel-delete-to-next-visible))
          (pel-delete-to-next-visible)
        (error "Function pel-delete-to-next-visible is not loaded")))))

;;-pel-autoload
(defun pel-unindent-lines (&optional n)
  "Un-indent current line or marked lines by N indentation levels.
Works when point is anywhere on the line.
All lines touched by the region are un-indented.
If region was marked, the function does not deactivate it to allow
repeated execution of the command.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines.
Limitation: does not handle hard tabs.
Handles presence of hard tabs:
- If `indent-tabs-mode' is non-nil the indentation is created with a mix
  of hard-tabs and space characters.
- If `indent-tabs-mode' is nil, any hard tab in the indentation of the
  marked lines is replaced by the proper number of spaces.
  Hard tabs after first non-whitespace character on the line are left."
  (interactive "*P")
  (let ((has-hard-tab (pel-indent-hard-tab-in-region-or-line-p)))
    (if (use-region-p)
        (let ((original-region-spec (pel-indent-marked-lines))
              (begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; Normally, Emacs deactivates the region right after a command
              ;; has executed.  To prevent this, and allow region to stay
              ;; active and visible for further execution of this command,
              ;; set the deactivate-mark variable to nil for this form.
              (deactivate-mark nil))
          (when has-hard-tab
              (pel-indent-untabify-region original-region-spec))
          (deactivate-mark)
          (goto-char begin-point)
          (dotimes (_i line-count)
            (pel--line-unindent n)
            (forward-line 1))
          (when indent-tabs-mode
            (pel-indent-tabify-region original-region-spec))
          (pel-indent-mark-lines-by-spec original-region-spec))
      ;; handle single line
      (when has-hard-tab
          (pel-indent-untabify-current-line))
      (pel--line-unindent n)
      (when indent-tabs-mode
        (pel-indent-tabify-current-line)))))

;;-pel-autoload
(defun pel-indent-rigidly (&optional n)
  "Indent rigidly the marked region or current line N times.
If a region is marked, it uses `indent-rigidly' and provides the
same prompts to control indentation changes.
If no region is marked, it operates on current line(s) identified
by the numeric argument N (or if not specified N=1):
- N = [-1, 0, 1]   : operate on current line
- N > 1   : operate on the current line and N-1 lines below.
- N < -1  : operate on the current line and (abs N) -1 lines above."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (unless (use-region-p)
      (if (= n 0)
          (setq n 1))
      (if (and (require 'pel-mark nil :no-error)
               (fboundp 'pel-mark-line-up)
               (fboundp 'pel-mark-line-down))
          (if (< n 0)
              (pel-mark-line-up n)
            (pel-mark-line-down n))
        (error "The pel-mark functions are not loaded")))
    (indent-rigidly (region-beginning) (region-end) nil t)))


(defconst pel--c-basic-offset-modes '(c-mode
                                      c++-mode
                                      objc-mode
                                      java-mode
                                      idl-mode
                                      pike-mode
                                      awk-mode
                                      d-mode
                                      tcl-mode) ; [:todo 2025-04-30, by Pierre
                                                ; Rouleau: not sure about tcl]
  "Major modes implemented as cc-modes.")


;;-pel-autoload
(defun pel-indent-control-context ()
  "Capture & return the indentation context for current major mode.
The returned value is a symbol -> value hash.
The symbols are:
- used-major-mode
- pel-insert-symbol-content-context-buffer
- isa-cc-mode
- isa-sh-mode
- pel-indentation-width-control-variables
- pel-indentation-other-control-variables
- "
  (let ((context (make-hash-table)))
    ;; (puthash 'used-major-mode           major-mode context)
    (puthash 'pel-insert-symbol-content-context-buffer
             (current-buffer) context)
    (puthash 'standard-indent   standard-indent context)
    (puthash 'tab-always-indent tab-always-indent context)
    (puthash 'indent-line-function indent-line-function context)
    (puthash 'isa-cc-mode
             (derived-mode-p pel--c-basic-offset-modes)
             context)
    (puthash 'isa-sh-mode (derived-mode-p '(sh-mode)) context)
    (puthash 'pel-indentation-width-control-variables
             pel-indentation-width-control-variables context)
    (puthash 'pel-indentation-other-control-variables
             pel-indentation-other-control-variables context)
    (puthash 'indent-indent-info-inserter-fct
             (intern
              (pel-string-with-major-mode
               "pel-%s-insert-indent-info"))
             context)
    (puthash 'pel-MM-indent-width
             (pel-major-mode-symbol-for "pel-%s-indent-width") context)
    context))

(defun pel-indent-insert-control-info (context)
  "Insert information related to the indentation control.

CONTEXT, a hash created by `pel-indent-control-context', captures the values of
important variables and symbols in the context of the inspected major mode."
  ;; 1- restore the context in let-bound variables.
  (let* ((pel-insert-symbol-content-context-buffer
          (gethash
           'pel-insert-symbol-content-context-buffer
           context))
         (standard-indent (gethash 'standard-indent context))
         (tab-always-indent (gethash 'tab-always-indent context ))
         (indent-line-function (gethash 'indent-line-function context))
         (isa-cc-mode     (gethash 'isa-cc-mode     context))
         (isa-sh-mode     (gethash 'isa-sh-mode     context))
         (pel-indentation-width-control-variables
          (gethash 'pel-indentation-width-control-variables context))
         (pel-indentation-other-control-variables
          (gethash 'pel-indentation-other-control-variables context))
         (indent-indent-info-inserter-fct
          (gethash 'indent-indent-info-inserter-fct context))
         (pel-MM-indent-width (gethash 'pel-MM-indent-width context))
         (already-inserted nil)
         (major-mode-specific-inserted nil))
    ;; 2- insert information using those values
    (insert (propertize "* Indentation Control:" 'face 'bold))
    ;;    - insert mode specialized info if a function exists for it.
    (when (fboundp indent-indent-info-inserter-fct)
      (setq already-inserted (funcall indent-indent-info-inserter-fct))
      (setq major-mode-specific-inserted t))
    ;;    - insert the generic info after; the ones that have not been
    ;;      already inserted by the mode specialized function as identified
    ;;      by the list of symbol it returns.
    (unless (memq 'pel-MM-indent-width already-inserted)
      (when (boundp pel-MM-indent-width)
        (pel-insert-symbol-content-line pel-MM-indent-width)
        (setq major-mode-specific-inserted t)))
    (when major-mode-specific-inserted
      (unless (memq 'precedence-info already-inserted)
        (insert "\n
Note: the above PEL and major-mode specific user options take precedence
      over the following variables, unless these are set by file variables:")))
    (unless (memq 'indent-description-info already-inserted)
      (when isa-cc-mode
        (pel-insert-symbol-content-line 'c-basic-offset))
      (when isa-sh-mode
        (pel-insert-symbol-content-line 'sh-basic-offset)))
    (dolist (symb '(standard-indent
                    tab-always-indent
                    indent-line-function))
      (unless (memq symb already-inserted)
        (pel-insert-symbol-content-line symb)))
    ;; --
    (unless (fboundp indent-indent-info-inserter-fct)
      ;; If there is no specialized inserter for this mode
      ;; but PEL has identified the variables that have an
      ;; impact on the indentation, insert information about
      ;; those here.
      (when pel-indentation-width-control-variables
        (insert "
 Indentation is controlled by the following variables:")
        (dolist (varsymb (if (listp pel-indentation-width-control-variables)
                             pel-indentation-width-control-variables
                           (list pel-indentation-width-control-variables)))
          (pel-insert-symbol-content-line varsymb)))
      (when pel-indentation-other-control-variables
        (insert "
 These other variables control various aspect of indentation:")
        (dolist (varsymb pel-indentation-other-control-variables)
          (pel-insert-symbol-content-line varsymb))))))


(defun pel-tab-control-context ()
  "Capture & return tab control context for current major mode.
The returned value is a symbol -> value hash.
The symbols are:
- used-major-mode
- pel-insert-symbol-content-context-buffer
- pel-tab-width-control-variables
- indent-tabs-mode : the value of indent-tabs-mode
- tab-width
- indent-tab-info-inserter-fct"
  (let ((context (make-hash-table)))
    (puthash 'used-major-mode           major-mode context)
    (puthash 'pel-insert-symbol-content-context-buffer (current-buffer) context)
    (puthash 'pel-tab-width-control-variables     pel-tab-width-control-variables context)
    (puthash 'indent-tabs-mode     indent-tabs-mode context)
    (puthash 'tab-width            tab-width context)
    (puthash 'tab-stop-list        tab-stop-list context)
    (puthash 'indent-tab-info-inserter-fct
             (intern
              (pel-string-with-major-mode
               "pel-%s-insert-tab-info"))
             context)
    (puthash 'pel-MM-tab-width
             (pel-major-mode-symbol-for "pel-%s-tab-width") context)
    (puthash 'pel-MM-use-tabs
             (pel-major-mode-symbol-for "pel-%s-use-tabs") context)
    (puthash 'pel-MM-tie-indent-to-tab-width
             (pel-major-mode-symbol-for "pel-%s-tie-indent-to-tab-width") context)
    (puthash 'pel--MM-indent-predef-vars
             (pel-major-mode-symbol-for "pel--%s-indent-predef-vars") context)
    context))

(defun pel-tab-insert-control-info (context)
  "Insert information related to the hard tab control.

CONTEXT, a hash created by `pel-tab-control-context', captures the values of
important variables and symbols in the context of the inspected major mode."
  ;; 1- restore the context in let-bound variables.
  (let* ((used-major-mode (gethash 'used-major-mode context))
         (pel-insert-symbol-content-context-buffer
          (gethash
           'pel-insert-symbol-content-context-buffer
           context))
         (pel-tab-width-control-variables     (gethash 'pel-tab-width-control-variables context))
         (indent-tabs-mode          (gethash 'indent-tabs-mode context))
         (tab-width                 (gethash 'tab-width context))
         (tab-stop-list             (gethash 'tab-stop-list context))
         (indent-tab-info-inserter-fct (gethash
                                        'indent-tab-info-inserter-fct
                                        context))
         (pel-MM-tab-width (gethash 'pel-MM-tab-width context))
         (pel-MM-use-tabs  (gethash 'pel-MM-use-tabs context))
         (pel-MM-tie-indent-to-tab-width (gethash
                                          'pel-MM-tie-indent-to-tab-width
                                          context))
         (pel--MM-indent-predef-vars (gethash
                                          'pel--MM-indent-predef-vars context))
         (already-inserted nil)
         (mode-base (pel-file-type-for used-major-mode)))
    ;; 2- insert information using those values
    (insert "\n\n")
    (insert (propertize "* Hard Tab Control:" 'face 'bold))
    (when (fboundp indent-tab-info-inserter-fct)
      (setq already-inserted (funcall indent-tab-info-inserter-fct)))
    (when (and (not (memq 'tab-description-intro already-inserted))
               (boundp pel-MM-tab-width))
      (insert (format "
- The hard tab rendering width is for %s buffer is controlled by
  `pel-%s-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in
  columns) that Emacs uses to render a hard tab character.
  Whether hard tabs are used in %s buffer is controlled by
  `pel-%s-use-tabs' and stored inside `indent-tabs-mode'.
" mode-base mode-base mode-base mode-base)))
    (unless (memq 'pel-MM-tab-width already-inserted)
      (when (boundp pel-MM-tab-width)
        (pel-insert-symbol-content-line pel-MM-tab-width)))
    (unless (memq 'pel-MM-use-tabs already-inserted)
      (when (boundp pel-MM-use-tabs)
        (pel-insert-symbol-content-line pel-MM-use-tabs)))
    (dolist (symb '(tab-width
                    indent-tabs-mode
                    tab-stop-list))
      (unless (memq symb already-inserted)
        (pel-insert-symbol-content-line symb)))
    (unless (memq 'pel-set-tab-width-description already-inserted)
      (insert (substitute-command-keys
               "

 You can use the `pel-set-tab-width' command via \\[pel-set-tab-width] to
 change the width rendering of hard tabs temporarily in the current buffer.
 If the indentation width is made equal to the `tab-width' then you can
 quickly change the rendering of the indentation using that command.")))
    (insert (format "

 Also note that:
  - The `pel-set-tab-width' command sets either `tab-width' or the variable
    identified inside `pel-tab-width-control-variables' if any.  PEL sets
    the buffer local value of `pel-tab-width-control-variables' when the buffer
    is opened.
  - For this mode its value is: pel-tab-width-control-variables : %S"
                    pel-tab-width-control-variables))
    (when (boundp pel-MM-tie-indent-to-tab-width)
      (insert "
  - That buffer local value is controlled by customization by:")
      (pel-insert-symbol-content-line pel-MM-tie-indent-to-tab-width)
      (unless (memq 'technique-to-use-hard-tab already-inserted)
        (insert "\n
     Although the use of hard tab for indentation is not popular for
     most programming languages, using hard tabs only for indentation
     allows full control of the visual rendering of indentation without
     any impact on the file content once the file has been tabified:
     just change the tab-width to make indentation appear narrower or wider.

     However, that technique only works nicely when all indentation are equal
     to a multiple number of the `tab-width' value.  If some indentation
     control variables differ by some amount, then spaces and hard tabs
     will be used and changing the tab width will impose insertion or removal
     of space characters when re-indenting.")
        (if (and pel-tab-width-control-variables
                 (not (pel-indent--indent-vars-have-offset
                       pel-tab-width-control-variables)))
            (insert (format "
     - For this buffer you can use this technique, given the current variables
       identified in pel-tab-width-control-variables."))
          (when (and (boundp pel--MM-indent-predef-vars)
                     (symbol-value pel--MM-indent-predef-vars))
            (if (not (pel-indent--indent-vars-have-offset
                      (symbol-value pel--MM-indent-predef-vars)))
                (insert (format "
     - For this mode, if you set `%s'
       to use-predef-vars, you will be able to use this technique
       in the next buffers you open."
                                pel-MM-tie-indent-to-tab-width))
              (insert (format "
     - For this mode, if you set `%s'
       to a list of indent variables with 0 for their offset,
       then you will be able to use this technique in the next
       buffers you open."
                              pel-MM-tie-indent-to-tab-width)))))))))


(defun pel-indent--indent-vars-have-offset (vars)
  "Return t if any indentation target variable is identified with an offset.

The VARS parameter must be either:
- a symbol,
- a list of symbols
- a list of (symbol . offset) cons cells.

The function return t only if there is at least one cons cell that
specifies a non-zero offset for a variable bound in the current mode."
  (let ((has-offset nil))
    (dolist (var (pel-list-of vars))
      (when (and (consp var)
                 (boundp (car var))
                 (not (eq (cdr var) 0)))
        (setq has-offset t)))
    has-offset))

;;-pel-autoload
(defun pel-show-indent (&optional append)
  "Display current buffer's indentation behaviour controlling variable state."
  (interactive "P")
  (let ((indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-indent-info*"
     "Indentation Width Control and Space/Tab Insertion Rendering"
     (lambda ()
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here

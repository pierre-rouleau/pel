;;; pel-indent.el --- PEL indentation utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-11-11 16:33:55 EST, updated by Pierre Rouleau>

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
;; This file provides several features related to indentation control.
;;
;;* Rigid Indentation Support
;;
;; Meant to be used in files where the super useful automatic indentation of
;; the function `indent-for-tab-command' is used and assigned to the tab key.
;;
;; Forcing rigid indentation is needed in several scenarios in major modes
;; such as C and other curly bracket programming languages as well as for
;; indentation sensitive programming languages like Python.
;;
;; 3 commands are provided:
;;
;;   - `pel-indent-lines'
;;   - `pel-unindent-lines'
;;   - `pel-indent-rigidly'
;;
;;   The `pel-indent-lines' command rigidly indent, and `pel-unindent-lines'
;;   rigidly un-indent the region by a specified number of indentation levels
;;   which defaults to 1.
;;
;;   The command `pel-indent-rigidly' allows interactive indentation by 1
;;   character at a time or indentation level at a time.
;;
;;   The number of columns used for the indentation level used by the
;;   functions `pel-indent-lines' and 'pel-unindent-lines' is returned by the
;;   function `pel-indent-level-colums'.
;;
;;   The function `pel-indent-lines' and `pel-unindent-lines' handle hard tabs
;;   properly according to the currently active `indent-tabs-mode'.
;;
;;   - When hard tabs are not permitted (i.e. `indent-tabs-mode' is nil), the
;;     function replace all hard tabs in the indentation by the appropriate
;;     number of space characters.  The functions do not replace hard tabs
;;     that are somewhere else on the line (e.g. inside a code string).
;;   - When hard tabs are permitted (i.e. `indent-tabs-mode' is t), the
;;     functions tabify the line or region marked.
;;
;;    TODO: enhance pel-indent-level-columns to better consider various types
;;          of files for various programming and markup languages.
;;
;;
;;* Control Tab Width
;;
;; Provide the `pel-set-tab-width' command.  This command sets the `tab-width'
;; value as well as the indentation control variable or variables used by the
;; current major mode.  By changing both the tab width and indentation width
;; control variables it becomes possible to change the visual rendering of the
;; indentation width when the indentation level width corresponds to the hard
;; tab with.
;;
;; The challenge comes from the fact that Emacs does not impose a strategy for
;; indentation to major modes.  Fortunately most major modes implementation
;; use a variable that controls the width of the indentation.  Some major
;; modes define several variables that sometimes all have the same value,
;; sometimes their value differ by an offset from the main indentation control
;; variable.
;;
;; PEL sets the content of the `pel-tab-width-control-variables' variable with
;; a list of the symbol of variables used by the major mode to control
;; indentation width and possibly an offset from the tab width.  The
;; `pel-set-tab-width' command sets `tab-width' to the requested value and
;; also sets the values of the identified variables to the same value or the
;; value with the identified offset.  This way the `pel-set-tab-width' command
;; can control the tab width and the indentation width.
;;
;;
;;* Indentation and Tab Width Control Introspection
;;
;; The `pel-show-indent' command prints information about the indentation and
;; tab width control for the current major mode.  It lists the control
;; variables used by the major mode and guides the user on what can be done in
;; this mode.
;;
;; The command uses several functions that can also be used by the mode
;; specialized commands named `pel-MM-indent-tab-info' where MM is replaced by
;; the major mode name.  If command exists for the current major mode,
;; `pel-show-indent' defers controls to that instead.  PEL provides several
;; such specialized functions.
;;
;; The utility functions can also be used by other commands such as the
;; `pel-mode-setup-info' provided by pel-modes.el
;;
;; When printing information about a major mode, the value of tjhe buffer
;; local variables must be first captured in a context.  In Lisp, lambda
;; expressions are often used as closures to do that.  Unfortunately this
;; technique cannot be used here because the function that insert the text are
;; sometimes used directly by the command sometimes used in other
;; functions. The solution is to capture the variable and their values inside
;; a hash that represents the context and use that in the function that prints
;; the information inside the help mode buffer.  Two functions capture such
;; context: `pel-indent-control-context' and `pel-tab-control-context'.  They
;; are called by the top level command where they call those in the context of
;; the inspected major mode and passing the resulting hash structure to the
;; printing function.
;;
;;
;;* Hard-tab Based Indentation Control
;;
;; Although not popular in most software development circles, using hard tabs
;; for indentation provides the undeniable advantage of flexibility in terms
;; of visual rendering.  Once all indentation level correspond to 1 hard-tab
;; it becomes very easy to change the visual width of indentation by simply
;; changing the rendered width of a hard tab character and that does not
;; modify the content of the file.
;;
;; This is a feature that appeals to people that have problems working with
;; small indentation width as increasing being reported on the Internet.  To
;; them the hard-line strict guidelines imposed by programming communities
;; such as Dart and Gleam who impose a 2-space indentation scheme is a real
;; problem.
;;
;; If the indentation scheme content of the files in those programming
;; languages cannot be changed as imposed by these draconian rules, a
;; workaround is to temporary change the indentation scheme to a hard-tab
;; based indentation and then change the width of the hard tab as well as the
;; width of all indentation control variables for the mode.
;;
;; For example, Dart and Gleam impose a 2-space indentation level.  For
;; buffers using major modes for those languages, we can use the following
;; procedure:
;;
;; - set the indentation control variable to 2 and the `tab-width' to 2,
;; - tabify the indentation whitespace of the entire buffer excluding all
;;   strings and comments.
;; - change the with of hard tabs (controlled by the `tab-width' variable),
;;   and the width of the variables for the major mode to a larger value.
;;
;; Once this is done, we can see the code with a wider indentation and
;; continue to work with the rules imposed by the major mode logic.
;;
;; Later, before saving the buffer back to the file, we simply perform the
;; following steps:
;;
;; - restore the tab and indentation width back to 2,
;; - untabify all indentation whitespace,
;; - saving the file.
;;
;; All necessary functions are provided here, along with a special minor-mode
;; that automatically performs all operation seamlessly, allowing editing the
;; Dart and Gleam files with wider indentation just as if they had flexible
;; guidelines.  The files will always retain their original indentation scheme
;; rigidity and everybody might be happier!

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)                    ; use: `pel-string-ends-with-p',
                                        ; `pel-file-type-for', `pel-list-of'
(require 'pel--indent)                  ; use: `'
(require 'pel--options)                 ; use: `pel-use-dtrt-indent'

(require 'simple)                       ; use: `indent-tabs-mode',
;;                                      ;      `normal-auto-fill-function'
;;; --------------------------------------------------------------------------
;;; Code:
;; ---------------------------------------------------------------------------
;;* Rigid Indentation Support
;; ---------------------------
;;
;;  * `pel-indent-lines'
;;    - pel-indent-hard-tab-in-region-or-line-p
;;      - pel-indent-current-line-positions
;;    - pel-indent-marked-lines
;;    - pel-indent-mark-lines-by-spec
;;      - pel-indent-mark-lines
;;        - pel-indent-line-pos
;;    - pel--insert-c-indent-line
;;      - pel-indent-level-columns
;;    - pel-indent-tabify-region
;;      - pel-indent-line-pos
;;    - pel-indent-untabify-region
;;      - pel-indent-line-pos
;;    - pel-indent-untabify-current-line
;;      - pel-indent-current-line-positions
;;    - pel-indent-tabify-current-line
;;      - pel-indent-current-line-positions
;;  * `pel-unindent-lines'
;;    - pel-indent-hard-tab-in-region-or-line-p
;;      - pel-indent-current-line-positions
;;    - pel-indent-marked-lines
;;    - pel--line-unindent
;;      - pel-indent-level-columns
;;        - pel--insert-c-indent-line
;;      - pel-indent-level-columns
;;    - pel-indent-tabify-region
;;      - pel-indent-line-pos
;;    - pel-indent-untabify-region
;;      - pel-indent-line-pos
;;    - pel-indent-untabify-current-line
;;      - pel-indent-current-line-positions
;;    - pel-indent-tabify-current-line
;;      - pel-indent-current-line-positions
;;  * `pel-indent-rigidly'

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

;; ---------------------------------------------------------------------------
;;* Control Tab Width
;; -------------------

;; This code uses the following defvar-local variables defined in the
;; pel--indent.el file:
;; - `pel-indentation-width-control-variables':
;;     - Identifies variables that play a role in the indentation
;;       and hold a width value.
;;     - used by functions that print information about the major mode.
;;     - set by pel_keys.el major mode setup logic.
;;
;; - `pel-indentation-other-control-variables'
;;     - Identifies variables that play a role in the indentation
;;       but are not a width value.
;;     - used by functions that print information about the major mode.
;;     - set by pel_keys.el major mode setup logic.
;;
;; - `pel-tab-width-control-variables'
;;     - Identifies variables that play a role in the indentation
;;       width that must be set by `pel-set-tab-width' when changing
;;       the width of hard tabs for indentation purpose.
;;
;; The call hierarchy is:
;;
;;  * `pel-set-tab-width'
;;    - pel-read-number
;;    - pel-mode-indent-control-vars
;;      - pel-mode-or-ts-mode-indent-control-vars


(defvar-local pel--original-tab-width nil
  "Tab width value used before `pel-indent-with-tabs' is used.")

(defvar-local pel--last-set-tab-width nil
  "Tab width set by last `pel-set-tab-width' call.")

(defun pel-read-number (prompt default history-symbol)
  "Emacs version sensitive `read-number'."
  (with-no-warnings
    (if pel-emacs-28-or-later-p
        (read-number prompt default history-symbol)
      (read-number prompt default))))

(defun pel-set-tab-width (n)
  "Set the tab width used in current buffer to the value N.

The change is temporary and affects the current buffer only.
Return the new `tab-width' or nil if unchanged."
  (interactive (list (pel-read-number "New tab-width: " tab-width
                                      'pel-set-tab-width-history)))
  (let ((control-vars (pel-mode-indent-control-vars))
        (current-tab-width tab-width)
        (offset nil))
    ;;
    (while (not (and (< n 9) (> n 1)))
      (setq n (pel-read-number "Enter valid tab-width in 2-8 range: "
                               current-tab-width
                               'pel-set-tab-width-history)))
    ;;
    (when (not (= n current-tab-width))
      (message "Changed buffer's tab-width from %d to %d" current-tab-width n)
      (when control-vars
        (dolist (var control-vars)
          (if (consp var)
              ;; a (symbol . offset)
              (progn
                (setq offset (cdr var))
                (setq var (car var))
                (when (boundp var)
                  (set (make-local-variable var) (+ n offset))))
            ;; just a symbol
            (when (boundp var)
              (set (make-local-variable var) n)))))
      ;; Always set `tab-width' to the new value.
      (setq-local tab-width n)
      (setq-local pel--last-set-tab-width n))))

;; ---------------------------------------------------------------------------
;;* Indentation and Tab Width Control Introspection
;; -------------------------------------------------
;;
;;  * `pel-show-indent'
;;     - pel-indent-insert-control-info
;;       . pel-indent-control-context
;;         - pel-mode-indent-control-vars
;;           - pel-mode-or-ts-mode-indent-control-vars
;;     - pel-tab-insert-control-info
;;       . pel-tab-control-context
;;       - pel-indent--indent-vars-have-offset
;;
;;  * `pel-indent-with-tabs-mode'
;;     - pel-mode-indentation-width
;;       - pel-mode-indent-control-vars
;;           - pel-mode-or-ts-mode-indent-control-vars
;;     - pel--install-indented-with-tabs-auto-fill
;;       - pel-indented-with-tabs-do-auto-fill
;;         - pel--adjusted-fill-column
;;     - pel--restore-original-fill-function
;;     - pel--tm-before-save-or-kill
;;     - pel--tm-after-save

;; Credit Note: the following table was originally derived from code
;;              that resides inside dtrt-indent.el and indent-control.el
;;        See:  https://github.com/jscheid/dtrt-indent
;;              https://github.com/jcs-elpa/indent-control
(defconst pel--mode-indent-vars
  ;; Mode            Syntax        Variable
  '((actionscript-mode   actionscript-indent-level)
    (ada-mode            ada-indent)    ; Ada
    (ada-ts-mode         ada-ts-mode-indent-offset)
    (apache-mode         apache-indent-level)
    (awk-mode            c-basic-offset)
    (bash-ts-mode        sh-basic-offset) ; Shell Script - use SMIE if available
    (c-mode              c-basic-offset)  ; C
    (c-ts-mode           c-ts-mode-indent-offset)
    (c++-mode            c-basic-offset) ; C++
    (c++-ts-mode         c-ts-mode-indent-offset)
    (cmake-mode          cmake-tab-width) ; CMake
    (cmake-ts-mode       cmake-ts-mode-indent-offset)
    (coffee-mode         coffee-tab-width)
    (coq-mode            coq-indent-basic)
    (cperl-mode          cperl-indent-level) ; Perl
    (cperl-mode          cperl-indent-level)
    (crystal-mode        crystal-indent-level) ; Crystal (Ruby) - use SMIE if available
    (csharp-mode         (c-basic-offset csharp-mode-indent-offset))
    (css-mode            css-indent-offset) ; CSS - use SMIE if available
    (less-css-mode       css-indent-offset)
    (scss-mode           css-indent-offset)
    (ssass-mode          ssass-tab-width)
    (dart-mode           tab-width)
    (dart-ts-mode        dart-ts-mode-indent-offset)
    (dockerfile-mode     dockerfile-indent-offset)
    (d-mode              c-basic-offset) ; D
    (elixir-mode         elixir-smie-indent-basic)
    (elm-mode            elm-indent-offset)
    (emacs-lisp-mode     lisp-body-indent)
    (enh-ruby-mode       enh-ruby-indent-level)
    (erlang-mode         erlang-indent-level) ; Erlang
    (ess-mode            ess-indent-offset)
    (f90-mode            (f90-associate-indent
                          f90-continuation-indent
                          f90-critical-indent
                          f90-do-indent
                          f90-if-indent
                          f90-program-indent
                          f90-type-indent))
    (feature-mode        (feature-indent-offset
                          feature-indent-level))
    (fsharp-mode         (fsharp-continuation-offset
                          fsharp-indent-level
                          fsharp-indent-offset))
    (gdscript-mode       gdscript-indent-offset)
    (go-ts-mode          go-ts-mode-indent-offset)
    (gpr-ts-mode         gpr-ts-mode-indent-offset)
    (groovy-mode         groovy-indent-offset) ; Groovy
    (jenkinsfile-mode    groovy-indent-offset)
    (haskell-mode        (haskell-indent-spaces
                          haskell-indent-offset
                          haskell-indentation-layout-offset
                          haskell-indentation-left-offset
                          haskell-indentation-starter-offset
                          haskell-indentation-where-post-offset
                          haskell-indentation-where-pre-offset
                          shm-indent-spaces))
    (haxe-mode           c-basic-offset)
    (haxor-mode          haxor-tab-width)
    (idl-mode            c-basic-offset)
    (jade-mode           jade-tab-width)
    (java-mode           c-basic-offset) ; Java
    (java-ts-mode        java-ts-mode-indent-offset)
    (jde-mode            c-basic-offset) ; Java (JDE)
    (javascript-mode     js-indent-level)
    (js-mode             js-indent-level) ; JavaScript
    (js-ts-mode          js-indent-level)
    (js-json-mode        js-indent-level)  ; JSON
    (js2-mode            js2-basic-offset) ; JavaScript-IDE
    (js2-jsx-mode        (js2-basic-offset sgml-basic-offset))
    (js3-mode            js3-indent-level) ; JavaScript-IDE
    (json-mode           js-indent-level)  ; JSON
    (json-ts-mode        json-ts-mode-indent-offset)
    (julia-mode          julia-indent-offset)
    (kotlin-mode         kotlin-tab-width)
    (lisp-mode             lisp-body-indent)
    (lisp-interaction-mode lisp-body-indent)
    (livescript-mode       livescript-tab-width)
    (lua-mode            lua-indent-level) ; Lua
    (magik-mode          magik-indent-level)
    (matlab-mode         matlab-indent-level)
    (meson-mode          meson-indent-basic)
    (mips-mode           mips-tab-width)
    (mustache-mode       mustache-basic-offset)
    (nasm-mode           nasm-basic-offset)
    (nginx-mode          nginx-indent-level)
    (nxml-mode           (nxml-child-indent nxml-attribute-indent))
    (objc-mode           c-basic-offset) ; Objective C
    (octave-mode         octave-block-offset)
    (nxml-mode           nxml-child-indent)   ; XML
    (pascal-mode         pascal-indent-level) ; Pascal
    (perl-mode           perl-indent-level)   ; Perl
    (php-mode            c-basic-offset)      ; PHP
    (pike-mode           c-basic-offset)
    (plantuml-mode       plantuml-indent-level) ; PlantUML
    (protobuf-mode       c-basic-offset)        ; Protobuf
    (pug-mode            pug-tab-width)         ; Pug
    (puppet-mode         puppet-indent-level)
    (ps-mode             ps-mode-tab)
    (python-mode         (python-indent-offset
                          py-indent-offset ; used by the badly maintained python-mode.
                          python-indent-levels))
    (raku-mode           raku-indent-offset) ; Perl6/Raku
    (rjsx-mode           (js-indent-level sgml-basic-offset))
    (ruby-mode           ruby-indent-level)     ; Ruby - use SMIE if available
    (enh-ruby-mode       enh-ruby-indent-level) ; Ruby - use SMIE if available
    (rust-mode           rust-indent-offset)    ; Rust - use SMIE if available
    (rust-ts-mode        rust-ts-mode-indent-offset)
    (rustic-mode         rustic-indent-offset) ; Rust - use SMIE if available
    (scala-mode          scala-indent:step)    ; Scala - use SMIE if available
    (sgml-mode           sgml-basic-offset)    ; SGML
    (shader-mode         shader-indent-offset)
    (slim-mode           slim-indent-offset)
    (sml-mode            sml-indent-level)
    (sql-mode            sql-indent-offset)
    (svelte-mode         svelte-basic-offset)
    (sh-mode             sh-basic-offset) ; Shell Script - use SMIE if available
    (swift-mode          swift-mode:basic-offset) ; Swift
    (tcl-mode            (tcl-indent-level tcl-continued-indent-level))
    (terra-mode          terra-indent-level)
    (typescript-mode     typescript-indent-level) ; Typescript
    (typescript-ts-base-mode typescript-ts-mode-indent-offset)
    (verilog-mode        (verilog-indent-level
                          verilog-indent-level-behavioral
                          verilog-indent-level-declaration
                          verilog-indent-level-module
                          verilog-cexp-indent
                          verilog-case-indent))
    (vhdl-mode           vhdl-basic-offset) ; VHDL
    (web-mode            (web-mode-attr-indent-offset
                          web-mode-attr-value-indent-offset
                          web-mode-code-indent-offset
                          web-mode-css-indent-offset
                          web-mode-markup-indent-offset
                          web-mode-sql-indent-offset
                          web-mode-block-padding
                          web-mode-script-padding
                          web-mode-style-padding)) ; HTML
    (xquery-mode         xquery-mode-indent-width) ; XQuery
    (yaml-mode           yaml-indent-offset)       ; YAML
    (zig-mode            zig-indent-offset))
  "Map mode name to indentation control variable(s) it uses.")


(defun pel-mode-or-ts-mode-indent-control-vars (&optional mode)
  "Return list of indentation control vars for classic/TS mode or MODE.
Return nil if nothing found for either."
  (let* ((mode (or mode major-mode))
         (vars (cadr (assoc mode pel--mode-indent-vars))))
    ;; If nothing is found for the -ts-mode, check if there is something for
    ;; the corresponding -mode since they are often tied together. If the
    ;; classic and TS mode use different variables there should be 2 entries
    ;; in the above table.
    (unless vars
      (when (pel-string-ends-with-p (symbol-name mode) "-ts-mode")
        (setq mode (intern (format "%s-mode" (pel-file-type-for mode))))
        (setq vars (cadr (assoc mode pel--mode-indent-vars)))))
    (pel-list-of vars)))

(defun pel-mode-indent-control-vars (&optional mode)
  "Return list of indentation control vars for current major mode or MODE.
Check the variables identified in `pel-tab-width-control-variables' if there is
some, otherwise check the variables defined in `pel--mode-indent-vars.'
Return nil if none is identified in either location."
  (let ((vars pel-tab-width-control-variables))
    (unless vars
      (setq vars (pel-mode-or-ts-mode-indent-control-vars mode)))
    (pel-list-of vars)))

(defun pel-mode-indentation-width (&optional mode)
  "Return the indentation width used by current major mode or MODE.
Return the value of the indentation control variable used for the
current major mode (or the specified MODE) if there is one.  If there
are several, return the value of the first one. Return the value of
`standard-indent' otherwise."
  (let ((vars (pel-mode-indent-control-vars mode)))
    (if vars
        (symbol-value (car vars))
      standard-indent)))

;;-pel-autoload
(defun pel-indent-control-context ()
  "Capture & return the indentation context for current major mode.
The returned value is a symbol -> value hash.
The symbols are:
- pel-insert-symbol-content-context-buffer
- standard-indent
- tab-always-indent
- indent-line-function
- the-indent-control-vars and all of those
- pel-indentation-width-control-variables
- pel-indentation-other-control-variables
- "
  (let ((context (make-hash-table)))
    (puthash 'pel-insert-symbol-content-context-buffer
             (current-buffer) context)
    (puthash 'standard-indent   standard-indent context)
    (puthash 'tab-always-indent tab-always-indent context)
    (puthash 'indent-line-function indent-line-function context)
    (puthash 'the-indent-control-vars (pel-mode-indent-control-vars) context)
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
          (gethash 'pel-insert-symbol-content-context-buffer context))
         (standard-indent (gethash 'standard-indent context))
         (tab-always-indent (gethash 'tab-always-indent context ))
         (indent-line-function (gethash 'indent-line-function context))
         (the-indent-control-vars (gethash 'the-indent-control-vars context))
         (pel-indentation-width-control-variables
          (gethash 'pel-indentation-width-control-variables context))
         (pel-indentation-other-control-variables
          (gethash 'pel-indentation-other-control-variables context))
         (indent-indent-info-inserter-fct
          (gethash 'indent-indent-info-inserter-fct context))
         (pel-MM-indent-width (gethash 'pel-MM-indent-width context))
         (already-inserted nil)
         (major-mode-specific-inserted nil)
         (pel-controls-indentation nil))
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
        (push pel-MM-indent-width already-inserted)
        (setq pel-controls-indentation t)
        (setq major-mode-specific-inserted t)))
    (when the-indent-control-vars
        (dolist (var the-indent-control-vars)
          (unless (memq var already-inserted)
            (pel-insert-symbol-content-line var)
            (push var already-inserted)
            (setq major-mode-specific-inserted t))))
    (when major-mode-specific-inserted
      (unless (memq 'precedence-info already-inserted)
        (if pel-controls-indentation
            (insert (format "\n
Note: `%s' controls indentation for new files as PEL uses
      its value and stores it in the other variables for
      the mode shown above.
" pel-MM-indent-width))
          (insert "\n
Note: The above variable control the indentation of this major mode.
      It takes precedence over the variables listed below.
"))
        (when pel-use-dtrt-indent
          (insert "\
      However, `dtrt-indent-mode' may detect a different indentation
      scheme for already written files and change the indentation
      control variable value used by the major-mode, overriding the
      value selected by customization.  In that case you will see a
      different value in the above list and a message note describing
      the adjustment made by `dtrt-indent-mode'.
"))))
    (dolist (symb '(standard-indent
                    tab-always-indent
                    indent-line-function
                    pel-indent-with-tabs-mode))
      (unless (memq symb already-inserted)
        (pel-insert-symbol-content-line symb)))
    ;; --
    (unless (fboundp indent-indent-info-inserter-fct)
      ;; If there is no specialized inserter for this mode
      ;; but PEL has identified the variables that have an
      ;; impact on the indentation, insert information about
      ;; those here.
      (when pel-indentation-width-control-variables
        (dolist (varsymb (if (listp pel-indentation-width-control-variables)
                             pel-indentation-width-control-variables
                           (list pel-indentation-width-control-variables)))
          (unless (memq varsymb already-inserted)
            (pel-insert-symbol-content-line varsymb))))
      (when pel-indentation-other-control-variables
        (dolist (varsymb pel-indentation-other-control-variables)
          (unless (memq varsymb already-inserted)
                  (pel-insert-symbol-content-line varsymb)))))))


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
      (insert (substitute-command-keys "

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
            (let ((pel-indent-with-tabs-mode-for-MM
                   (pel-major-mode-symbol-for "pel-indent-with-tabs-mode-for-%s")))
              (insert
               (format "
     - For this buffer you can use this technique, because the current variables
       identified in pel-tab-width-control-variables identify variables that
       have an offset of zero from the tab width.
       - Use the `pel-indent-with-tabs-mode' for that.%s"
                       (if (boundp pel-indent-with-tabs-mode-for-MM)
                           (format "
         The easiest way is to set `%s' to the indentation width
         you want to use in the buffer to automatically activate this minor
         mode which convert indentation when opening and saving the file."
                                   pel-indent-with-tabs-mode-for-MM)
                         "")))
              (when (boundp pel-indent-with-tabs-mode-for-MM)
                (pel-insert-symbol-content-line pel-indent-with-tabs-mode-for-MM)))
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

;;-pel-autoload
(defun pel-show-indent (&optional append)
  "Display current buffer's indentation behaviour controlling variable state."
  (interactive "P")
  (let ((indent-tab-info-cmd (intern (pel-string-with-major-mode
                                      "pel-%s-indent-tab-info"))))
    (if (fboundp indent-tab-info-cmd)
        (call-interactively indent-tab-info-cmd)
      (let ((indent-control-context (pel-indent-control-context))
            (tab-control-context (pel-tab-control-context)))
        (pel-print-in-buffer
         "*pel-indent-info*"
         "Indentation Width Control and Space/Tab Insertion Rendering"
         (lambda ()
           (pel-indent-insert-control-info indent-control-context)
           (pel-tab-insert-control-info tab-control-context))
         (unless append :clear-buffer)
         :use-help-mode)))))

;; ---------------------------------------------------------------------------
;;* Hard-tab Based Indentation Control
;;
;;   * `pel-indent-with-tabs-mode', the minor mode.
;;     * `pel-indent-with-tabs'
;;       - pel-tabify-all-indent
;;         - pel-inside-code
;;       . `pel-set-tab-width'
;;     * `pel-indent-with-spaces'
;;       . `pel-set-tab-width'

(defun pel-inside-code (&optional pos)
  "Return non-nil when point is in code, nil if in comment or string.
Note that this changes the search match data!"
  (let* ((pos (or pos (point)))
         (syntax (syntax-ppss pos)))
    (and (not (nth 3 syntax))
         (not (nth 4 syntax)))))

(defun pel-tabify-all-indent ()
  "Convert multiple spaces in indent to tabs when possible.

Process complete buffer: a group of spaces in the leading indentation is
partially replaced by tabs when this can be done without changing the
column they end at.  Comments and strings are not modified.

The variable `tab-width' controls the spacing of tab stops.
This is a indentation specific `tabify' function."
  (save-excursion
    (save-restriction
      ;; Process entire buffer.
      (goto-char (point-min))
      (let ((indent-tabs-mode t)
            (inside-code nil))
        (while (re-search-forward "^[ \t]* [ \t]+" nil t)
          ;; In white-space indentation: adjust to TABs were possible.
          (save-match-data
            (setq inside-code (pel-inside-code (point))))
          (when inside-code
            (let ((end-col (current-column))
                  (beg-col (save-excursion (goto-char (match-beginning 0))
                                           (skip-chars-forward "\t")
                                           (current-column))))
              (unless (= (/ end-col tab-width) (/ beg-col tab-width))
                ;; The spacing (after some leading TABs which we wouldn't
                ;; want to touch anyway) does not straddle a TAB boundary,
                ;; so it neither contains a TAB, nor will we be able to use
                ;; a TAB here anyway: there's nothing to do.
                (delete-region (match-beginning 0) (point))
                (indent-to end-col)))))))))

(defvar pel-indent-with-tabs-mode)      ; prevent byte compiler warning

(defun pel-indent-with-tabs (&optional with-tab-width
                                       by-minor-mode)
  "Convert current buffer to use tabs for indentation.

If the optional WITH-TAB-WIDTH numerical argument is specified, after
conversion to tab-based indentation change the tab width to that
specified value.  If the argument is not specified, prompt for the tab
width to use.

This command is only available when the `pel-indent-with-tabs-mode' is
turned off.  Since it is used internally by `pel-indent-with-tabs-mode',
the BY-MINOR-MODE parameter must only be set by the call from
`pel-indent-with-tabs-mode'."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (pel-read-number "Indent with tab width: "
                            tab-width
                            (pel-major-mode-symbol-for
                             "pel-indent-with-tabs-history-for-%s")))))
  (if (or by-minor-mode (not pel-indent-with-tabs-mode))
      (progn
        ;; first tabify indentation whitespace, replacing space-based
        ;; indentation with tabs that represent the specified tab width.
        (pel-tabify-all-indent)
        ;; Remember `tab-width' originally used in the buffer.
        ;; It should correspond with the indentation width.
        (unless pel--original-tab-width
          (setq-local pel--original-tab-width tab-width))
        ;; Adjust the tab and indentation width to the new selection.
        (pel-set-tab-width with-tab-width)
        ;; New indented code must now be indented with hard tabs.
        (indent-tabs-mode 1))
    (user-error
     "Command not available while pel-indent-with-tabs-mode is active!")))

(defun pel-indent-with-spaces (&optional with-tab-width by-minor-mode)
  "Convert current buffer to use space for indentation.

Restore the space-based indentation scheme using the tab width that was
used before the first call to `pel-indent-with-tabs' unless the optional
WITH-TAB-WIDTH numerical argument is specified.  If an optional
numerical argument is specified, use that for tab width.

This command is only available when the `pel-indent-with-tabs-mode' is
turned off.  Since it is used internally by `pel-indent-with-tabs-mode',
the BY-MINOR-MODE parameter must only be set by the call from
`pel-indent-with-tabs-mode'."
  (interactive "P")
  (if (or by-minor-mode (not pel-indent-with-tabs-mode))
      (save-excursion
        (if with-tab-width
            (pel-set-tab-width with-tab-width)
          ;; Restore the original tab-width if it was stored in
          ;; `pel--original-tab-width'
          (when (or  pel--original-tab-width
                     pel--last-set-tab-width)
            (pel-set-tab-width pel--original-tab-width)))
        ;; Then untabify.  Note that hard-tabs inside strings and comments
        ;; will be replaced by spaces.  If this is a problem in some cases,
        ;; please let me know.
        (untabify (point-min) (point-max))
        ;; New indented code must now be indented with spaces.
        (indent-tabs-mode -1))
    (user-error
     "Command not available while pel-indent-with-tabs-mode is active!")))

;; ---------------------------------------------------------------------------
;;* Manage auto-fill in tab-based indented buffer
;;  ---------------------------------------------
;;
;; When a buffer is loaded with the content of a file that uses a 2-space
;; indentation scheme and a maximum line length of 80 columns, we need to
;; adjust the `fill-column' value when then buffer holds the text that uses a
;; different indentation based on tabs that are rendered with a different
;; width. The code in this section deals with that.
;;
;; The value of the original `fill-column' used for the space-based
;; indentation file is remembered in the `pel--normalfile-fill-column'
;; buffer local variable.
;;
;; When the `pel-mode' is active, it replaces the function that performs
;; the automatic filling by `pel--normalfile-fill-column' which computes
;; the adjusted value of fill-column on each line by counting the number of
;; hard tab character present on the line and their impact on the fill-column.
;; That function is only called when automatic filling is activated.
;;
;; These are only used indirectly by the `pel-mode' as shown by the
;; following call hierarchy, where
;; `pel--install-indented-with-tabs-auto-fill' installs that function to
;; deal with automatic filling and `pel--restore-original-fill-function'
;; restores the original function when turning off `pel-mode':
;;
;;  * `pel-indent-with-tabs-mode'
;;    - `pel--install-indented-with-tabs-auto-fill'
;;      > `pel-indented-with-tabs-do-auto-fill'
;;      - `pel--adjusted-fill-column'
;;    - `pel--restore-original-fill-function'

(defvar-local pel--normalfile-fill-column nil
  "The fill-column value used for the normal space indented file format.")

(defun pel--adjusted-fill-column (space-indent-width viewed-tab-width
                                                     &optional position)
  "Return adjusted fill column for tab-indented line at POSITION or point.

That is the fill-column that can be used in the tab-indented buffer to
correspond to what `fill-column' is inside the real space-indented file.
- SPACE-INDENT-WIDTH corresponds to what the file normally uses.
- VIEWED-TAB-WIDTH corresponds to what is used in the buffer."
  (save-excursion
    (when position (goto-char position))
    (let* ((extra-columns-per-tab (- viewed-tab-width space-indent-width))
           (line-start-pos (progn (forward-line 0) (point)))
           (line-end-pos   (progn (end-of-line) (point)))
           (tab-count      (count-matches "\t" line-start-pos line-end-pos))
           (extra-columns  (* tab-count extra-columns-per-tab)))
      ;; Cache the real, file-specific, `fill-column' value in buffer local
      ;; variable.
      (unless pel--normalfile-fill-column
        (setq-local pel--normalfile-fill-column fill-column))
      ;; return what fill column should be for this line
      (+ pel--normalfile-fill-column extra-columns))))

(defvar-local pel--normal-auto-fill-function nil
  "Remember auto-fill-function normally used for normal files.")

(defvar-local pel--space-based-indent-width nil
  "Original space based indentation width for the file.")

(defun pel-indented-with-tabs-do-auto-fill ()
  "Perform the auto-fill inside a tabs-indented buffer.
Adjust the buffer-local `fill-column' based on the indentation scheme used and
in the normal file and the tabs-based indentation used inside the buffer, then
  execute the `do-auto-fill' "
  ;; Adjust the fill-column to what it should be if the indentation had been
  ;; reconverted back to 2-space indents and then execute the fill function.
  (let ((fill-column (pel--adjusted-fill-column pel--space-based-indent-width
                                                tab-width)))
    (funcall pel--normal-auto-fill-function)))

(defun pel--install-indented-with-tabs-auto-fill ()
  "Install the tabs-indented aware auto fill function."
  ;; Cache the `auto-fill-function' for the buffer.
  (unless pel--normal-auto-fill-function
    (setq-local pel--normal-auto-fill-function normal-auto-fill-function)
    (make-local-variable 'normal-auto-fill-function)
    (setq-local normal-auto-fill-function
                'pel-indented-with-tabs-do-auto-fill))
  (when auto-fill-function
    (setq-local auto-fill-function
                (function pel-indented-with-tabs-do-auto-fill))))

(defun pel--restore-original-fill-function ()
  "Restore original fill function."
  (when pel--normal-auto-fill-function
    (setq-local normal-auto-fill-function pel--normal-auto-fill-function)
    (when auto-fill-function
      (setq-local auto-fill-function pel--normal-auto-fill-function))))


(defvar-local pel--tab-width-used-during-tab-based-indent nil)

(defun pel--tm-before-save-or-kill ()
  "Disable tab-based indentation and restore native space-base indent.
This is performed just before saving a buffer to a file or killing it."
  (setq-local pel--tab-width-used-during-tab-based-indent tab-width)
  (pel-indent-with-spaces nil :by-minor-mode))

(defun pel--tm-after-save ()
  "Restore tab-based indentation with same width used before buffer save."
  (if pel--tab-width-used-during-tab-based-indent
      (progn
        (pel-indent-with-tabs pel--tab-width-used-during-tab-based-indent
                              :by-minor-mode)
        (set-buffer-modified-p nil))
    (display-warning 'pel-indent
                     "pelt--tm-after-save: unknown indentation width!"
                     :error)))
;; ---------------------------------------------------------------------------
;;* Minor Mode
;;  ----------

(define-minor-mode pel-indent-with-tabs-mode
  "Minor mode that automatically converts buffer to tab-based indentation."
  :lighter " ⍈"
  (let ((warning-message-printed nil))
    (if pel-indent-with-tabs-mode
        ;; When turning mode on
        ;; --------------------
        (progn
          (if (eq tab-width (pel-mode-indentation-width))
              ;; conditions are met to transform buffer to tab-based indent
              (progn
                ;; if buffer is modified allow user to save first.
                ;; If user quit, catch and activate the mode anyway, without saving.
                (condition-case nil
                    (when (and (buffer-modified-p)
                               (y-or-n-p (format "Save modified %S first? "
                                                 (current-buffer))))
                      (save-buffer))
                  (quit
                   (message "Indenting with tabs Mode enabled, buffer not saved!")
                   (setq warning-message-printed t)))
                ;; Proceed
                (unless warning-message-printed
                  (message "Converting %s to tab-based indent, width=%d ..."
                           (current-buffer)
                           tab-width ))
                (with-silent-modifications
                  ;; Remember the original space based indentation width
                  (setq-local pel--space-based-indent-width
                              (pel-mode-indentation-width))

                  ;; activate indentation with tabs using either the indentation width
                  ;; specified by customization (if that symbol exists and is non-nil
                  ;; or the native tab-width matching indentation width
                  (pel-indent-with-tabs (or (pel-major-mode-symbol-value-or
                                             "pel-indent-with-tabs-mode-for-%s"
                                             nil)
                                            tab-width)
                                        :by-minor-mode)
                  ;; Install a special auto-fill function that is aware that each tab
                  ;; in the buffer corresponds to the file original space indentation
                  ;; scheme.
                  (pel--install-indented-with-tabs-auto-fill))
                ;; The buffer was modified by replacing spaces with tabs but
                ;; since we want to use it as if it was normal, don't show
                ;; the buffer modified unless it already was.
                (unless warning-message-printed
                  (set-buffer-modified-p nil))
                ;; schedule operation before and after buffer save.
                (unless (memq 'pel--tm-before-save-or-kill  before-save-hook)
                  (add-hook 'before-save-hook 'pel--tm-before-save-or-kill
                            -100
                            'local
                            ))
                (unless (memq 'pel--tm-before-save-or-kill  kill-buffer-hook)
                  (add-hook 'kill-buffer-hook 'pel--tm-before-save-or-kill
                            -100
                            'local
                            ))
                (unless (memq 'pel--tm-after-save after-save-hook)
                  (add-hook 'after-save-hook 'pel--tm-after-save
                            +100
                            'local))

                (unless warning-message-printed
                  (message "Indenting with tabs Mode enabled.")))
            ;; conditions are NOT met to transform buffer to tab-based indent
            ;; tab-width differs from current indentation!
            (setq-local pel-indent-with-tabs-mode nil)
            (user-error "\
Cannot activate pel-indent-with-tabs-mode: tab-width (%d) differs from %s (%d)!
These must be the same and must represent the real indentation width used.
To change tab-width, type:  M-: (setq-local tab-width %d)"
                        tab-width
                        (pel-mode-indent-control-vars)
                        (pel-mode-indentation-width)
                        (pel-mode-indentation-width))))

      ;; When turning mode off
      ;; ---------------------
      (with-silent-modifications
        (pel-indent-with-spaces nil :by-minor-mode))
      (pel--restore-original-fill-function)
      (when (memq 'pel--tm-before-save-or-kill before-save-hook)
        (remove-hook 'before-save-hook 'pel--tm-before-save-or-kill 'local))
      (when (memq 'pel--tm-before-save-or-kill kill-buffer-hook)
        (remove-hook 'kill-buffer-hook 'pel--tm-before-save-or-kill 'local))
      (when (memq 'pel--tm-after-save after-save-hook)
        (remove-hook 'after-save-hook 'pel--tm-after-save 'local))
      (message "Indenting with tabs Mode disabled."))))

;;; --------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here

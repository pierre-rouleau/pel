;;; pel-cc.el --- PEL support for CC modes  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-20 13:59:42 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020-2026  Pierre Rouleau
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
;;  This file contains utilities to help manage the various CC modes: AWK, C,
;;  C++, D, Objective-C and Pike major modes.

;; Insert line break suitable to the context
;; - `pel-cc-newline'
;;
;; Control of the RET key behaviour in cc-modes
;;  - `pel-cc-change-newline-mode'
;;
;; Indentation Control
;;  * `pel-cc-set-indent-width'
;;
;; C/C++ Comment Style Selection
;;  * `pel-c-toggle-comment-style'
;;
;; eldoc support for C
;;  * `pel-toggle-c-eldoc-mode'
;;
;; Show CC-Mode Setup/Status/Settings Information
;;  * `pel-cc-mode-info'
;;    - `pel-cc-bracket-style-for'
;;    - `pel-cc-c-default-style-for'
;;    - `pel-cc-electric-keys'
;;      - `pel-cc-filter-electric-key'
;;        - `pel-cc-key-electric-p'
;;
;; Major-mode dispatcher for .h files
;;  - `pel-cc-mode'
;;    - `pel-is-objective-c-buffer'
;;    - `pel-is-cpp-buffer'
;;      - `pel--isa-buffer-of'
;;
;; File Type identification utilities
;;  - `pel-is-objective-c-file'
;;    . `pel-is-objective-c-buffer'
;;  - `pel-is-cpp-file'
;;    . `pel-is-cpp-buffer'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)         ; use: `pel-current-buffer-starts-with'
(require 'pel--options)
(require 'pel-ffind)         ; use: `pel-ffind-project-rootdir'
(require 'cc-vars)           ; use: `c-basic-offset'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Insert line break suitable to the context
;;  =========================================

(defvar-local pel-cc-newline-mode 'context-newline
  "Operation mode of the RET key in CC mode.
One of:
- context-newline       : use function `c-context-line-break'
- newline-and-indent    : use function `newline'
- just-newline-no-indent: use function `electric-indent-just-newline'")

;;-pel-autoload
(defun pel-cc-newline (&optional n)
  "Insert a newline and perhaps align.

With argument N repeat N times.

For newline insertion, operate according to the value of the
variable `pel-cc-newline-mode'.

If the variable `pel-newline-does-align' is t, then perform the
text alignment done by the function `align'."
  (interactive "*P")
  (require 'align)
  (require 'pel-align)
  (let ((separate (or (when (boundp 'align-region-separate)
                        (if (and (symbolp align-region-separate)
                                 (boundp align-region-separate))
                            (symbol-value align-region-separate)
                          align-region-separate))
		      'entire))
	(end      (point))
        (mode-used
         (cond
          ((and (eq pel-cc-newline-mode 'context-newline)
                (fboundp 'c-context-line-break))
           (dotimes (_i (prefix-numeric-value n))
             (c-context-line-break))
           'context-newline)
          ;;
          ((and (eq pel-cc-newline-mode 'just-newline-no-indent)
                (fboundp 'electric-indent-just-newline))
           (electric-indent-just-newline n)
           'just-newline-no-indent)
          ;;
          (t ; simple.el is always loaded so default to newline
           (newline n :interactive)
           'newline))))
    (when (and (boundp 'pel-newline-does-align)
               pel-newline-does-align
               (fboundp 'align-new-section-p)
               (fboundp 'align))
      (save-excursion
        (forward-line -1)
        (while (not (or (bobp)
                        (align-new-section-p (point) end separate)))
          (forward-line -1))
        (align (point) end)))
    mode-used))

;;* Control of the RET key behaviour in cc-modes
;;  ============================================

;;-pel-autoload
(defun pel-cc-change-newline-mode ()
  "Change the way the RET key behaves in the CC modes.

Change behaviour of function `pel-cc-newline', and display the new mode
in the echo area.

Changes the mode to the next mode with the following order:
- context newline       <--+
- newline and indent       |
- just newline no indent ->+

Display and return the new value of the mode."
  (interactive)
  (prog1
      (setq pel-cc-newline-mode
            (cond ((eq pel-cc-newline-mode 'context-newline)
                   'newline-and-indent)
                  ((eq pel-cc-newline-mode 'newline-and-indent)
                   'just-newline-no-indent)
                  (t
                   'context-newline)))
    (message "Return key now does: %S" pel-cc-newline-mode)))

;; ---------------------------------------------------------------------------
;;* Indentation Control
;;  ===================

;;-pel-autoload
(defun pel-cc-set-indent-width (&optional new-width)
  "Interactively change the Indentation width for current buffer to NEW-WIDTH.

Prompt if not specified (in interactive execution) or when nil is passed
explicitly.
Accepts values between 2 and 8 inclusively to set the width.
When a language indent width default number is configured, type 0 or RET
to restore it.
Does not change the indentation with of other buffers."
  (interactive)
  ;; Prompt when called interactively (new-width is always nil then) or
  ;; when nil is passed programmatically.
  (let* ((original-indent-width c-basic-offset)
         (default-indent
          (pel-major-mode-symbol-value-or "pel-%s-indent-width" nil))
         (used-default (or default-indent c-basic-offset))
         (show-default (and (numberp used-default)
                            (not (eq used-default original-indent-width)))))

    (when (or (null new-width)
              (called-interactively-p 'any))
      (setq new-width
            (read-number (format "Indentation width [%s]%s: "
                                 original-indent-width
                                 (if show-default
                                     " 0 or RET to restore default"
                                   ""))
                         ;; In some modes, `c-basic-offset' is 'set-from-style
                         ;; for those do not provide the default because its
                         ;; not a number, for the others provide the default
                         ;; in case user hit RET to accept the default.
                         (when show-default
                           used-default))))
    (cond
     ((< 1 new-width 9)
      (setq-local c-basic-offset new-width))
     ((and default-indent
           (= new-width 0))
      (setq-local c-basic-offset default-indent))
     (t (user-error "Enter a positive value in the range [2,8]%s, not %s!"
                    (if default-indent " (or 0 to restore default)" "")
                    new-width)))
    (if (eq original-indent-width new-width)
        (message "No change to indent width; keep %s" original-indent-width)
      (message "indentation is now %s" c-basic-offset))))

;; ---------------------------------------------------------------------------
;;* C/C++ Comment Style Selection
;;  =============================
;;
;; Select between /* C */ and // C++ style comments.
;;
;; Note: this does not support the Objective-C JavaDoc/Doxygen and the
;; HeaderDoc styles.  These could be added to C and C++ as well.
;;
;; /**
;;  * Javadoc/Doxygen Objective-C style.
;;  * Calculates the area.
;;  * @param radius The radius of the circle.
;;  * @return The area.
;;  */
;;
;;
;; /*!
;;  * @brief A brief description of the method using HeaderDoc style.
;;  */

;;-pel-autoload
(defun pel-c-toggle-comment-style (&optional arg)
  "Toggle the comment style between block and line comments.
Calls `c-toggle-comment-style' and print a message describing
the newly used comment style.

Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or left out.

This action does nothing when the mode only has one comment style."
  (interactive "P")
  (require 'cc-cmds)
  (when (fboundp 'c-toggle-comment-style)
    (c-toggle-comment-style arg)
    (message "Now commenting with: %s %s" comment-start comment-end)))

;; ---------------------------------------------------------------------------
;;* eldoc support for C
;;  ===================

;;-pel-autoload
(defun pel-toggle-c-eldoc-mode ()
  "Toggle c-eldoc mode on/off."
  (interactive)
  (if (pel-eglot-active-p)
      (user-error "This buffer uses eglot; that has built-in eldoc support!")
    (require 'c-eldoc nil :noerror)
    (when (fboundp 'c-turn-on-eldoc-mode)
      (if eldoc-mode
          (progn
            (eldoc-mode -1)
            (message "eldoc-mode now OFF."))
        (c-turn-on-eldoc-mode)
        (message "c-eldoc-mode now ON. ")))))

;; ---------------------------------------------------------------------------
;;* Show CC-Mode Setup/Status/Settings Information
;;  ==============================================

(defun pel-cc-key-electric-p (key)
  "Return non-nil if KEY is electric, nil otherwise."
  ;; Work only with keys that may be electric.
  (local-key-binding key))

(defun pel-cc-filter-electric-key (char)
  "Return CHAR if it is electric, nil otherwise."
  (if (pel-cc-key-electric-p (kbd char))
      char
    nil))

(defun pel-cc-electric-keys ()
  "Return a string with the electric keys."
  (seq-filter 'pel-cc-filter-electric-key
          (mapcar #'string "#*/<>(){}:;,")))

(defun pel-cc-c-default-style-for (mode)
  "Return styles identified in variable `c-default-style' for MODE.
- Return \"unbound, not loaded\" if `c-default-style' is not bound.
- Return a list of style strings in order of entry in `c-default-style' if
  it holds something for the MODE.
- Return \"no style associated with mode\" if `c-default-style' has no
  association for the MODE."
  (if (boundp 'c-default-style)
      (let ((styles ()))
        (dolist (mode.style c-default-style)
          (if (eq (car mode.style) mode)
              (push (cdr mode.style) styles)))
        (if styles
            (nreverse styles)
          (format "no style associated with %s" mode)))
    "unbound, not loaded"))

(defun pel-cc-bracket-style-for (mode)
  "Return the name of the PEL requested bracket style for MODE."
  ;; Strip trailing "-mode" suffix (5 chars): c-mode -> c, c++-mode -> c++
  (let* ((mode-str (substring (symbol-name mode) 0 -5))
         (symbol-name (intern (format "pel-%s-bracket-style" mode-str))))
    (if (boundp symbol-name)
        (symbol-value symbol-name)
      (format "unknown - %s is void" symbol-name))))

;;-pel-autoload
(defun pel-cc-mode-info (&optional append)
  "Display information about current CC mode derivative in specialized buffer.

By default, clear previous buffer content and show the top of buffer.
If APPEND argument is non-nil, append to the previous report and show the top
of the last report."
  (interactive "P")
  (let* ((is-c (eq major-mode 'c-mode))
         (pel-insert-symbol-content-context-buffer (current-buffer))
         (not-avail-msg "not available for this mode")
         (info
          (format
           "%s
- active style        : %s. This c-default-style is %s.
- Electric characters : %s
- RET mode            : %S%s
                        Toggle indent on RET with `pel-toggle-newline-indent-align' with %s.
- Auto newline        : %s
- fill column         : %s%s
- Tab width           : %-19s Set via: %s(%s)    ==> tab-width(%s)          when %s buffer is opened.
- Indentation chars   : %-19s Set via: %s(%3s)   ==> indent-tabs-mode(%3s) when %s buffer is opened.
- Indent width        : %-19s Set via: %s
- Syntactic indent    : %s
- c-indentation-style : %s
- PEL bracket style   : %s
- Comment style       : %s
- Hungry delete       : %s
- Project root        : %s
- File searching info : type  %s in %s buffer to get more information about file searching mechanism."
           (pel-as-bold (format "%s state:" major-mode)) ; 1
           (if (boundp 'c-default-style) ; 2
               (alist-get major-mode c-default-style)
             "Unknown - c-default-style not loaded")
           ;; Electric characters
           (pel-symbol-on-off-string 'c-electric-flag ; 3
                                     (format "active on: %s characters"
                                             (pel-concat-strings-in-list
                                              (pel-cc-electric-keys)))
                                     "inactive"
                                     not-avail-msg)
           (pel-cc-c-default-style-for major-mode) ; 4
           pel-cc-newline-mode                     ; 5
           (pel-symbol-on-off-string 'pel-newline-does-align
                                     ", and aligns (comments, assignments, etc...)"
                                     ", and does not align"
                                     "")                             ; 6
           (pel-key-binding-string 'pel-toggle-newline-indent-align) ; 7
           ;; Auto newline
           (pel-symbol-on-off-string 'c-auto-newline nil nil not-avail-msg) ; 8
           fill-column                  ; 9
           (pel-symbol-value-or 'auto-fill-function
                                ""
                                (lambda (aff-symbol)
                                  (let ((auto-filling (symbol-value aff-symbol)))
                                    (if auto-filling
                                        (format ", auto-fill active: done by %S."
                                                auto-filling)
                                      ", auto-filling: off."))))
           ;; Tab width
           tab-width                                       ; 11
           (pel-string-with-major-mode "pel-%s-tab-width") ; 12
           (pel-symbol-value-or 'pel-c-tab-width)          ; 13
           (pel-symbol-value-or 'tab-width)                ; 14
           major-mode                                      ; 15
           ;; Indentation chars
           (pel-on-off-string indent-tabs-mode ; 16
                              "hard-tabs & spaces"
                              "spaces only")
           (pel-string-with-major-mode "pel-%s-use-tabs") ; 17
           (pel-symbol-value-or 'pel-c-use-tabs)          ; 18
           (pel-symbol-value-or 'indent-tabs-mode)        ; 19
           major-mode                                     ; 20
           ;; Indent width
           (pel-symbol-value-or 'c-basic-offset) ; 21
           (if (eq (pel-major-mode-symbol-value "pel-%s-indent-width")
                   (pel-symbol-value-or 'c-basic-offset)) ; 22
               (format "%s(%s) ==> c-basic-offset(%s)     when %s buffer is opened."
                       (pel-string-with-major-mode "pel-%s-indent-width")
                       (pel-major-mode-symbol-value "pel-%s-indent-width")
                       (pel-symbol-value-or 'c-basic-offset)
                       major-mode)
             (format
              "c-basic-offset(%s) overridden in buffer (by pel-cc-set-indent-width ?)."
              (pel-symbol-value-or 'c-basic-offset)))
           ;; Syntactic indent
           (pel-symbol-on-off-string    ; 23
            'c-syntactic-indentation nil nil not-avail-msg)
           ;; c-indentation-style
           (pel-symbol-value-or 'c-indentation-style) ; 24
           ;; PEL bracket style
           (pel-cc-bracket-style-for major-mode)   ; 25
           ;; Comment style
           (if (and (boundp 'c-block-comment-flag) ; 26
                    (boundp 'c-block-comment-starter)
                    (boundp 'c-block-comment-ender)
                    (boundp 'c-block-comment-prefix))
               (if c-block-comment-flag
                   (format
                    "Block (C-style) comments: %s %s , continued line start with %s"
                    c-block-comment-starter
                    c-block-comment-ender
                    c-block-comment-prefix)
                 (format "Line (C++-style) comments: %s" (pel-symbol-value-or
                                                          'c-line-comment-starter)))
             not-avail-msg)
           ;; Hungry delete
           (pel-symbol-on-off-string 'c-hungry-delete-key ; 27
                                     nil
                                     (substitute-command-keys "\
off, but the \\[c-hungry-delete-forward] and \\[c-hungry-delete-backwards] keys and their <f11> bindings are available.")
                                     not-avail-msg)
           ;; Project root
           (or (pel-ffind-project-rootdir) ; 28
               "None found")
           ;; File searching info
           (pel-key-binding-string 'pel-ffind-show-status) ; 29
           (current-buffer))))
    (pel-print-in-buffer
     (pel-string-with-major-mode "*pel-%s-info*")
     (pel-string-with-major-mode "*%s Control")
     (lambda ()
       "Print abbreviation control variables."
       (insert info)
       ;; provide control access
       (insert "\n")
       (pel-insert-mode-symbol-content-line "pel-%s-activates-minor-modes")
       (pel-insert-symbol-content-line 'pel-cc-newline-mode)
       (pel-insert-symbol-content-line 'pel-modes-activating-align-on-return)
       ;; (pel-insert-symbol-content-line 'pel-newline-does-align)
       (pel-insert-mode-symbol-content-line "pel-%s-indent-width")
       (pel-insert-mode-symbol-content-line-when-bound "pel-%s-multiline-comments")
       (pel-insert-mode-symbol-content-line "pel-%s-tab-width")
       (pel-insert-mode-symbol-content-line "pel-%s-use-tabs")
       (pel-insert-symbol-content-line 'pel-ffind-executable)
       (pel-insert-symbol-content-line 'pel-use-smart-dash)

       (pel-insert-symbol-content-line 'c-default-style)
       (pel-insert-symbol-content-line 'c-electric-flag)
       (pel-insert-symbol-content-line 'auto-fill-function)
       (pel-insert-symbol-content-line 'c-line-comment-starter)
       (pel-insert-symbol-content-line 'c-block-comment-flag
                                       pel-insert-symbol-content-context-buffer
                                       (if (pel-symbol-value
                                            'c-block-comment-flag
                                            pel-insert-symbol-content-context-buffer)
                                           " : C-style /* comment */"
                                         " : C++-style // comment"))
       (pel-insert-symbol-content-line 'c-block-comment-starter)
       (pel-insert-symbol-content-line 'c-block-comment-ender)
       (pel-insert-symbol-content-line 'c-block-comment-prefix)

       (pel-insert-bold "\n\n*Pre-processor indentation control: ")
       (insert "(use c-toggle-cpp-indent-to-body, <f12> <f4> #, to toggle) ")
       (pel-insert-symbol-content-line 'c-cpp-indent-to-body-flag)
       (pel-insert-list-content 'c-cpp-indent-to-body-directives nil nil nil :on-same-line)
       (pel-insert-symbol-content-line 'c-electric-pound-behavior)

       (pel-insert-bold "\n\n*Style control: ")
       (insert "(use c-set-offset, C-c C-o, to modify)")
       (pel-insert-list-content  'c-syntactic-context)
       (pel-insert-list-content  'c-offsets-alist nil nil nil :on-same-line)

       (pel-insert-bold "\n\n*File extension association:")
       (pel-insert-symbol-content-line 'pel-auto-mode-alist)
       (pel-insert-bold "\n\n*File skeleton control:")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-use-separators")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-insert-file-timestamp")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-with-license")
       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for (format
                                    "pel-%%s-skel-c%sfile-section-titles"
                                    (if is-c "" "pp"))))
       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for (format
                                    "pel-%%s-skel-h%sfile-section-titles"
                                    (if is-c "" "pp"))))
       (pel-insert-mode-symbol-content-line "pel-%s-skel-doc-markup")
       (when is-c (pel-insert-symbol-content-line 'pel-c-skel-comment-with-2stars))
       (pel-insert-mode-symbol-content-line "pel-%s-skel-use-include-guards")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-module-header-block-style")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-insert-function-sections")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-function-section-titles")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-function-define-style")
       (pel-insert-mode-symbol-content-line "pel-%s-skel-function-name-on-first-column")
       (unless is-c
         (pel-insert-symbol-content-line 'pel-c++-class-has-doc-block)
         (pel-insert-symbol-content-line 'pel-c++-class-doc-section-titles)
         (pel-insert-symbol-content-line 'pel-c++-class-members-sections))
       (pel-insert-bold "\n\n*See Also:\n- ")
       (pel-insert-symbol 'auto-mode-alist)
       (insert "\n\n"))
     (unless append :clear-buffer)
     :use-help-mode
     (unless append :show-top))))

;; ---------------------------------------------------------------------------
;;* Major-mode dispatcher for .h files
;;  ==================================
;;
;; Distinguish Objective-C, C++ and C Code in files by looking for
;; language specific keywords.


(defconst pel-cpp-regexp
  (rx-to-string
   '(or
     ;; bare C++ keywords that need symbol boundaries
     (: symbol-start (or "final"
                         "override"
                         "virtual"
                         "class")
        symbol-end)
     ;; self-delimiting with one of: trailing space, ':', '::', '<',
     ;; or non-identifier chars:
     ;; "class "
     "namespace "
     "using "
     "private:"
     "protected:"
     "public:"
     "std::"
     "template<"
     "C++ FILE:"))
  "Regexp string to search for C++ distinguishing code.")

(defconst pel-objective-c-keywords
  '(
    "@interface"
    "@implementation"
    "@end"
    "@protocol"
    "@class"
    ;;
    "@property"
    "@synthesize"
    "@dynamic"
    "@selector"
    "@encode"
    ;;
    "@public"
    "@protected"
    "@private"
    "@package"
    ;;
    "@try"
    "@catch"
    "@finally"
    "@throw"
    "@synchronized"
    ;;
    "#import <"  ; '#import "fpath"' is supported by Microsoft C++ COM
    )
  "Symbols only found in Objective-C code.")

(defconst pel-objective-c-regexp (rx-to-string
                                  `(: (or ,@pel-objective-c-keywords)))
  "Regexp string to search for Objective-C distinguishing code.")

(defun pel--isa-buffer-of (lang-regexp)
  "Return t if the buffer hold the language identified by LANG-REGEXP.
Return nil otherwise."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward lang-regexp nil t))
        (save-match-data
          (let ((original-mode major-mode) ; Save current mode function
                ;; prevent major mode message output
                (inhibit-message t))
            (unwind-protect
                (progn
                  ;; temporary use C mode to allow detection of comments
                  ;; that are supported by all C, C++ and Objective-C
                  (c-mode)
                  (when (pel-inside-code (point))
                    (setq found t)))
              ;; Restore original mode
              (funcall original-mode)))))
      found)))

(defun pel-is-objective-c-buffer ()
  "Return t if current buffer holds Objective-C code, nil otherwise."
  (or (pel--isa-buffer-of pel-objective-c-regexp)
      (pel-current-buffer-starts-with "// OBJC FILE: ")))

(defun pel-is-cpp-buffer ()
  "Return t if current buffer holds C++ code, nil otherwise."
  (or (pel--isa-buffer-of pel-cpp-regexp)
      (pel-current-buffer-starts-with "// C++ FILE: ")))

;;-pel-autoload
(defun pel-cc-mode ()
  "Major mode dispatcher for C, C++ and Objective-C.
Use one of the following modes depending what is found inside the content of
the file and the values of `pel-use-c', `pel-use-c++' and `pel-use-objc'."
  (interactive)
  (cond
   ;; Objective-C (there is no Tree-Sitter support for Objective-C yet.)
   ((pel-is-objective-c-buffer)
    (objc-mode))
   ;; Note: Emacs uses `c-or-c++-mode' to distinguish C++ from C, but that
   ;;       searches everywhere, including inside comments and strings.
   ;; C++
   ((pel-is-cpp-buffer)
    (if (and  (eq pel-use-c++ 'with-tree-sitter)
              (fboundp 'c++-ts-mode))
        (c++-ts-mode)
      (c++-mode)))
   ;; Defaults to C
   (t
    (if (and (eq pel-use-c 'with-tree-sitter)
             (fboundp 'c-ts-mode))
        (c-ts-mode)
      (c-mode)))))

;;* File Type identification utilities
;;  ==================================

(defun pel-is-objective-c-file (fpath)
  "Return t if file at FPATH contains Objective-C code.
Return nil otherwise."
  (with-temp-buffer
    (insert-file-contents fpath)
    (pel-is-objective-c-buffer)))

(defun pel-is-cpp-file (fpath)
  "Return t if file at FPATH contains C++ code.
Return nil otherwise."
  (with-temp-buffer
    (insert-file-contents fpath)
    (pel-is-cpp-buffer)))

;;; --------------------------------------------------------------------------
(provide 'pel-cc)

;;; pel-cc.el ends here

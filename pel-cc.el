;;; pel-cc.el --- PEL support for CC modes.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-08-23 14:49:04 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022, 2023, 2024  Pierre Rouleau
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
;;  This file contains utilities to help manage the various CC modes.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)
(require 'pel-ffind)                    ; use: `pel-ffind-project-directory'
;;; --------------------------------------------------------------------------
;;; Code:
;;

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
text alignment done by the function `align'.

For Emacs Lisp code: return value of effective variable
`pel-cc-newline-mode' used.  The function may use something
different then expected if the function it should call is not
loaded. That would only occur if this function is called from a
mode not fully supported."
  (interactive "*P")
  (pel-require 'align)
  (pel-require 'pel-align)
  (let ((separate (or (when (boundp 'align-region-separate)
                        (if (and (symbolp align-region-separate)
                                 (boundp align-region-separate))
                            (symbol-value align-region-separate)
                          align-region-separate))
		      'entire))
	(end      (point))
        (mode-used
         (cond ((and (eq pel-cc-newline-mode 'context-newline)
                     (fboundp 'c-context-line-break))
                (dotimes (_i (prefix-numeric-value n))
                  (c-context-line-break))
                'context-newline)
               ((and (eq pel-cc-newline-mode 'just-newline-no-indent)
                     (fboundp 'electric-indent-just-newline))
                (electric-indent-just-newline n)
                'just-newline-no-indent)
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

;;-pel-autoload
(defun pel-cc-change-newline-mode ()
  "Change behaviour of function `pel-cc-newline'.

Changes the mode to the next mode with the following order:
+-> context newline -> newline and indent -> just newline no indent ->+
|----------------------<----------------------------------------------|

Display and return the new value of the mode."
  (interactive)
  (prog1
      (setq pel-cc-newline-mode (cond ((eq pel-cc-newline-mode 'context-newline)
                                       'newline-and-indent)
                                      ((eq pel-cc-newline-mode 'newline-and-indent)
                                       'just-newline-no-indent)
                                      (t
                                       'context-newline)))
    (message "Return key now does: %S" pel-cc-newline-mode)))

;; --

(defun pel-cc-key-electric-p (key)
  "Return non-nil if KEY is electric, nil otherwise."
  ;; Work only with keys that may be electric.
  (local-key-binding key))

(defun pel-cc-filter-electric-key (char)
  "Return CHAR if it is electric, space otherwise."
  (if (pel-cc-key-electric-p (kbd char))
      char
    nil))

(defun pel-cc-electric-keys ()
  "Return a string with the electric keys."
  (seq-filter 'pel-cc-filter-electric-key
          (mapcar 'string "#*/<>(){}:;,")))

(defun pel-cc-c-default-style-for (mode)
  "Return styles identified in variable `c-default-style' for MODE.
Return a list of style strings in order of entry in the
variable `c-default-style' if bound.  If it is unbound
return \"void\"."
  (if (boundp 'c-default-style)
      (let ((styles '()))
        (dolist (mode.style c-default-style (reverse styles))
          (if (eq (car mode.style) mode)
              (push (cdr mode.style) styles))))
    "void"))

(defun pel-cc-bracket-style-for (mode)
  "Return the name of the PEL requested bracket style for MODE."
  (let* ((mode-str (substring (symbol-name mode) 0 -5)) ; strip trailing "-mode"
         (symbol-name (intern (format "pel-%s-bracket-style" mode-str))))
    (if (boundp symbol-name)
        (symbol-value symbol-name)
      (format "unknown - %s is void" symbol-name))))

;;-pel-autoload
(defun pel-cc-mode-info (&optional append)
  "Display information about current CC mode derivative in specialized buffer.

Clear previous buffer content unless optional APPEND argument is non-nil,
in which case it appends to the previous report."
  (interactive "P")
  (let* ((is-c (eq major-mode 'c-mode))
         (pel-insert-symbol-content-context-buffer (current-buffer))
         (not-avail-msg "not available for this mode")
         (file-finder-method (pel-major-mode-symbol-value-or ;27
                              "pel-%s-file-finder-method" "(not supported)"))
         (info
          (format
           "%s state:
- active style        : %s. c-default-style: %s
- RET mode            : %S%s
- Electric characters : %s
- Auto newline        : %s
- fill column         : %s%s
- Tab width           : %-19s Set via: %s(%s)    ==> tab-width(%s)          when %s buffer is opened.
- Indentation chars   : %-19s Set via: %s(%3s)   ==> indent-tabs-mode(%3s) when %s buffer is opened.
- Indent width        : %-19s Set via: %s
- Syntactic indent    : %s
- c-indentation-style : %s
- PEL Bracket style   : %s
- Comment style       : %s
- Hungry delete       : %s
- Project root        : %s
- File finder method  : %s
- %s
- Extra Searched dirs : %s"
           major-mode                    ; 1
           (if (boundp 'c-default-style) ; 2
               (alist-get major-mode c-default-style)
             "Unknown - c-default-style not loaded")
           (pel-cc-c-default-style-for major-mode) ; 3
           pel-cc-newline-mode                     ; 4
           (pel-symbol-on-off-string 'pel-newline-does-align
                                     ", and aligns (comments, assignments, etc...)"
                                     ""
                                     "")              ; 5
           (pel-symbol-on-off-string 'c-electric-flag ; 6
                                     (format "active on: %s"
                                             (pel-concat-strings-in-list
                                              (pel-cc-electric-keys)))
                                     "inactive"
                                     not-avail-msg)
           (pel-symbol-on-off-string 'c-auto-newline nil nil not-avail-msg) ; 7
           fill-column                  ; 8
           (pel-symbol-value-or 'auto-fill-function
                                ""
                                (lambda (aff-symbol)
                                  (let ((auto-filling (symbol-value aff-symbol)))
                                    (if auto-filling
                                        (format ", auto-fill active: done by %S."
                                                auto-filling)
                                      ", auto-filling: off."))))
           ;; --
           tab-width                                       ; 10
           (pel-string-with-major-mode "pel-%s-tab-width") ; 11
           (pel-symbol-value-or 'pel-c-tab-width)          ; 12
           (pel-symbol-value-or 'tab-width)                ; 13
           major-mode                                      ; 14
           ;; --
           (pel-on-off-string indent-tabs-mode ; 15
                              "hard-tabs & spaces"
                              "spaces only")
           (pel-string-with-major-mode "pel-%s-use-tabs") ; 16
           (pel-symbol-value-or 'pel-c-use-tabs)          ; 17
           (pel-symbol-value-or 'indent-tabs-mode)        ; 18
           major-mode                                     ; 19
           ;; --
           (pel-symbol-value-or 'c-basic-offset) ; 20
           ;; --
           (if (eq (pel-major-mode-symbol-value "pel-%s-indent-width")
                   (pel-symbol-value-or 'c-basic-offset)) ; 21
               (format "%s(%s) ==> c-basic-offset(%s)     when %s buffer is opened."
                       (pel-string-with-major-mode "pel-%s-indent-width")
                       (pel-major-mode-symbol-value "pel-%s-indent-width")
                       (pel-symbol-value-or 'c-basic-offset)
                       major-mode)
             (format
              "c-basic-offset(%s) overridden in buffer (by pel-cc-set-indent-width ?)."
              (pel-symbol-value-or 'c-basic-offset)))
           ;; --
           (pel-symbol-on-off-string    ; 22
            'c-syntactic-indentation nil nil not-avail-msg)
           (pel-symbol-value-or 'c-indentation-style) ; 23
           (pel-cc-bracket-style-for major-mode)      ; 24
           (if (and (boundp 'c-block-comment-flag)    ; 25
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
           (pel-symbol-on-off-string 'c-hungry-delete-key ; 26
                                     nil
                                     "off, but the \
F11-⌦  and F11-⌫  keys are available."
                                     not-avail-msg)
           ;; TODO: move following code close to file finder logic
           (or (pel-ffind-project-directory) ; 27
               (format
                "None found, searching for files identified in pel-project-root-identifiers: %s"
                pel-project-root-identifiers))
           file-finder-method           ; 28
           (cond                        ; 29
            ((eq file-finder-method 'generic)
             (format "%-20s: %s"
                     " pel-ffind-executable"
                     pel-ffind-executable))
            ((eq file-finder-method 'pel-ini-file)
             (format "%-20s: %s"
                     " tool chain"
                     (or (pel-major-mode-symbol-value-or
                          "pel--%s-file-finder-ini-tool-name" "(not supported)")
                         "none specified")))
            ((stringp file-finder-method)
             (format
              " Search in path listed in environment variable %s"
              file-finder-method))
            ((listp file-finder-method)
             (format
              " Search in specific directories: %s" file-finder-method))
            (t "No file finder supported."))
           (or (pel-major-mode-symbol-value
                "pel-%s-file-searched-extra-dir-trees")
               ""))))
    (pel-print-in-buffer
     (pel-string-with-major-mode "*pel-%s-info*")
     (pel-string-with-major-mode "*%s Control")
     (lambda ()
       "Print abbreviation control variables."
       (insert info)
       ;; provide control access
       (insert "\n")
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel-%s-activates-minor-modes"))
       (pel-insert-symbol-content-line 'pel-cc-newline-mode)
       (pel-insert-symbol-content-line 'pel-modes-activating-align-on-return)
       ;; (pel-insert-symbol-content-line 'pel-newline-does-align)
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel-%s-indent-width"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel-%s-tab-width"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel-%s-use-tabs"))
       (unless (string= file-finder-method "(not supported)")
         (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                          "pel-%s-file-finder-method")))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel--%s-file-finder-ini-tool-name"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                        "pel-%s-file-searched-extra-dir-trees"))
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

       (insert "\n\n*Style control:")
       (pel-insert-list-content  'c-syntactic-context)
       (pel-insert-list-content  'c-offsets-alist nil nil nil :on-same-line)

       (insert "\n\n*File extension association:")
       (pel-insert-symbol-content-line 'pel-auto-mode-alist)
       (insert "\n\n*File skeleton control:")
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-use-separators"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-insert-file-timestamp"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-with-license"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for (format
                                                                   "pel-%%s-skel-c%sfile-section-titles"
                                                                   (if is-c "" "pp"))))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for (format
                                                                   "pel-%%s-skel-h%sfile-section-titles"
                                                                   (if is-c "" "pp"))))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-doc-markup"))
       (when is-c (pel-insert-symbol-content-line 'pel-c-skel-comment-with-2stars))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-use-include-guards"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-module-header-block-style"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-insert-function-sections"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-function-section-titles"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-function-define-style"))
       (pel-insert-symbol-content-line (pel-major-mode-symbol-for "pel-%s-skel-function-name-on-first-column"))
       (unless is-c
         (pel-insert-symbol-content-line 'pel-c++-class-has-doc-block)
         (pel-insert-symbol-content-line 'pel-c++-class-doc-section-titles)
         (pel-insert-symbol-content-line 'pel-c++-class-members-sections))
       (insert "\n\n See Also:\n- ")
       (pel-insert-symbol 'auto-mode-alist)
       )
     (unless append :clear-buffer)
     :use-help-mode)))

;; --

(defun pel-cc-set-indent-width (&optional new-width)
  "Interactively change the Indentation with for current buffer to NEW-WIDTH.

Prompt if not specified."
  (interactive "nIndentation width (0 to restore default): ")
  (if (boundp 'c-basic-offset)
      (progn
        (cond
         ((> new-width 0)
          (setq-local c-basic-offset new-width))
         ((= new-width 0)
          (setq-local c-basic-offset
                      (pel-major-mode-symbol-value "pel-%s-indent-width")))
         (t (user-error "Enter 0 or positive value!")))
        (message "indentation is now %s" c-basic-offset))
    (error "c-basic-offset is not loaded!")))

;;; --------------------------------------------------------------------------
(provide 'pel-cc)

;;; pel-cc.el ends here

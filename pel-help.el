;;; pel-help.el --- PEL extra help utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau (concat "prouleau" "001" "@" "gmail" ".com")

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
;;; Commentary:
;;
;; Contains a small set of help providing utility commands.
;;
;; They are all lazily loaded via use-package declarations in pel_keys.el,
;; therefore is no need to list them in the PEL autoloading list.

;; -----------------------------------------------------------------------------
;;; Dependency
(require 'pel--options)       ; use: pel-pdf-fire-url

;;; Code:

;;-pel-autoload
(defun pel-show-kill-ring ()
  "Display content of `kill-ring' in *Help* buffer.
Simple shortcut to invoke `describe-variable' on the `kill-ring' variable."
  (interactive)
  (describe-variable 'kill-ring))

;;-pel-autoload
(defun pel-show-major-mode ()
  (interactive)
  "Display the symbol of the current major mode."
  (message "Major mode: %S" major-mode))

;; -----------------------------------------------------------------------------

(defconst pel--prefix-to-topic-alist
  '(
    ([f11 97 f1]      . "abbreviations")
    ([f11 116 97 f1]  . "align")
    ([f11 44 f1]      . "auto-completion")
    ;; ([]            . "autosave-backup")
    ([f11 39 f1]      . "bookmarks")
    ([f11 98 f1]      . "buffers")
    ([f11 116 f1]     . "case-conversion")  ; text
    ([f11 116 f1]     . "text-modes")  ; text
    ;; ([f11 f1]      . "closing-suspending")
    ([f11 59 f1]      . "comments")
    ;; ([f11 f1]      . "completion-input")
    ([f11 99 f1]      . "counting")
    ;; ([f11 f1]      . "cua")
    ([f11 109 f1]     . "cursor")
    ([f11 f2 f1]      . "customize")
    ([f11 61 f1]      . "cut-paste")
    ([f11 45 f1]      . "cut-paste")
    ([f11 100 f1]     . "diff-merge")
    ([f11 108 f1]     . "display-lines")
    ([f11 68 f1]      . "drawing")
    ([f11 116 109 f1] . "enriched-text")
    ;; ([f11 f1]      . "ert")
    ;; ([f11 f1]      . "faces-fonts")
    ([f11 102 f1]     . "file-mngt")
    ([f11 102 118 f1] . "file-variables")
    ([f11 116 102 f1] . "filling-justification")
    ([f11 70 f1]      . "frames")
    ([f11 32 103 f1]  . "graphviz-dot")
    ([f12 f1]         . "graphviz-dot")
    ([f11 103 f1]     . "grep")
    ([f11 63 f1]      . "help")
    ([f11 59 f1]      . "hide-show-code")
    ([f11 98 104 f1]  . "highlight")
    ;; ([f11 f1]      . "hooks")
    ([f11 9 f1]       . "indentation")
    ([f11 116 f1]     . "input-method")
    ([f11 121 f1]     . "inserting-text")
    ([f11 105 f1]     . "inserting-text")
    ([f11 95 f1]      . "inserting-text")
    ;; ([f11 f1]      . "key-chords")
    ([f11 107 f1]     . "keyboard-macros")
    ;; ([f11 f1]      . "keys-f11")
    ;; ([f11 f1]      . "keys-fn")
    ;; ([f11 f1]      . "macOS-terminal-settings")
    ([f11 46 f1]      . "marking")
    ([f11 f10 f1]     . "menus")
    ([f11 102 f1]     . "mode-dired")
    ;; ([f11 f1]      . "mode-org-mode")
    ([f11 32 114 f1]  . "mode-rst")
    ([f12 f1]         . "mode-rst")
    ;; ([f11 f1]      . "modifier-keys")
    ;; ([f11 f1]      . "mouse")
    ;; ([f11 f1]      . "narrowing")
    ;; ([f11 f1]      . "navigation")
    ;; ([f11 f1]      . "numkeypad")
    ;; ([f11 f1]      . "packages")
    ([f11 32 97 f1]   . "pl-applescript")
    ([f12 f1]         . "pl-applescript")
    ([f11 32 67 f1]   . "pl-c++")
    ([f12 f1]         . "pl-c++")
    ([f11 32 99 f1]   . "pl-c")
    ([f12 f1]         . "pl-c")
    ([f11 32 76 f1]   . "pl-common-lisp")
    ([f12 f1]         . "pl-common-lisp")
    ([f11 32 100 f1]  . "pl-d")
    ([f12 f1]         . "pl-d")
    ([f11 32 120 f1]  . "pl-elixir")
    ([f12 f1]         . "pl-elixir")
    ([f11 32 108 f1]  . "pl-emacs-lisp")
    ([f12 f1]         . "pl-emacs-lisp")
    ([f11 32 101 f1]  . "pl-erlang")
    ([f12 f1]         . "pl-erlang")
    ([f11 32 102 f1]  . "pl-forth")
    ([f12 f1]         . "pl-forth")
    ([f11 32 106 f1]  . "pl-julia")
    ([f12 f1]         . "pl-julia")
    ([f11 32 112 f1]  . "pl-python")
    ([f12 f1]         . "pl-python")
    ([f11 32 82 f1]   . "pl-rexx")
    ([f12 f1]         . "pl-rexx")
    ([f11 32 f1]      . "plantuml")
    ([f12 f1]         . "plantuml")
    ([f11 112 f1]     . "projectile")
    ;; ([f11 f1]      . "rectangles")
    ([f11 114 f1]     . "registers")
    ([f11 124 f1]     . "scrolling")
    ([f11 115 f1]     . "search-replace")
    ([f11 83 f1]      . "sessions")
    ([f11 120 f1]     . "shells")
    ([f11 111 f1]     . "sorting")
    ([f11 27 115 f1]  . "speedbar")
    ([f11 36 f1]      . "spell-checking")
    ([f11 88 f1]      . "tags")
    ([f11 116 109 f1] . "text-modes")
    ([f11 116 116 f1] . "transpose")
    ([f11 117 f1]     . "undo-redo-repeat")
    ([f11 118 f1]     . "vsc-mercurial")
    ([f11 102 f1]     . "web")
    ([f11 116 119 f1] . "whitespaces")
    ([f11 119 f1]     . "windows"))
  "Map from key prefix array to topic string.")


;; --
(defun pel-major-mode-topic ()
  "Return the PEL topic name of the current major mode."
  ;; Unfortunately I did not name all PDF files in a regular
  ;; way for everything - I followed some conventions but I used
  ;; 3 different conventions instead of only one.
  (let ((mode-str (substring (symbol-name major-mode) 0 -5)))
    ;; mode-str contains the string before the "-mode" suffix
    ;; Must now convert to the conventions used by pel
    (cond
     ;; - first check if it's one of the programming languages
     ((member mode-str '("applescript"
                         "c++"
                         "c"
                         "common-lisp"
                         "d"
                         "elixir"
                         "emacs-lisp"
                         "erlang"
                         "forth"
                         "julia"
                         "python"
                         "rexx"))
      (format "pl-%s" mode-str))
     ;; - check if its a markup mode
     ((member mode-str '("dired"
                         "rst"))
      (format "mode-%s" mode-str))
     ;; - check others
     ((member mode-str '("graphviz-dot"
                         "plantuml"))
      mode-str))))


;;-pel-autoload
(defun pel-help-pdf ()
  (interactive)
  "Open the PEL PDF sheet for current PEL key prefix."
  (let* ((invoking-keys   (this-command-keys))
         (is-mode-specific (equal invoking-keys [f12 f1])))
    (dolist (keys.topic pel--prefix-to-topic-alist)
      (if is-mode-specific
          (let ((topic-name (pel-major-mode-topic)))
            ;; Identify the topic for the mode specific and open that PDF
            (if topic-name
                (browse-url
                 (pel-pdf-file-url
                  topic-name))
              (user-error "No available help PDF file for %s!" major-mode)))
        (when (equal (car keys.topic) invoking-keys)
          (browse-url
           (pel-pdf-file-url
            (cdr keys.topic))))))))

;;-pel-autoload
(defun pel-help-pdfs-dir ()
  "Open a Dired buffer on the PEL PDF directory."
  (interactive)
  ;; TODO: if the buffer is already opened, move point to that buffer and
  ;; make that buffer visible, don't open a new buffer or
  ;; don't use the current window
  (find-file (pel-pdf-directory)))

;; -----------------------------------------------------------------------------
(provide 'pel-help)

;;; pel-help.el ends here

;;; pel-applescript.el --- PEL AppleScript support -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

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


;;; Commentary:
;;
;; This is *only* available for macOS platforms.
;;
;; AppleScript support for Emacs running in either GUI or Terminal mode
;; on a macOS system.  The support does not allow more than just executing
;; an applescript command or program.  It does not support full interaction
;; with AppleScript via events.
;;
;; In Terminal mode, the do-applescript function is implemented invoking
;; the osascript in a child process instead of using the Cocoa built-in
;; library.
;;
;; Currently the main purpose of this is to allow the use of the vocal
;; interface and read text out-load, as this is a very early version of the
;; code.
;;
;; * pel-say-word
;; * pel-say-sentence
;; * pel-say-paragraph
;; * pel-say-region
;;   * pel-say
;;     - do-applescript
;; - pel-say-words
;;   - pel--convert-to-strings
;; - pel-run-applescript
;;

;;; Code:


(require 'pel-read)
(require 'pel--options)                 ; uses: pel-mac-voice-name
(require 'pel--base)

(defconst pel-narration-translations
  '(("[_(){}`*~\\<>/^•]" . " ")
    ("\""  .  "")
    (";"   . ".  ")        ; pause longer on semicolon
    ("--+" . "")           ; remove  underlining
    ("==+" . "")           ; remove underlining
    ("#"   . " pound ")) ; only supporting English (todo: support more)

  "List of (`input regexp` . `output text`).
Used by `pel-say' to help translate input text to help smooth the narration.
The translation identified in the first list element is done first.")


;; --

(if pel-system-is-macos-p
    (if (display-graphic-p)
        (require 'term/ns-win)
      (defun do-applescript (command)
        "Execute a small AppleScript COMMAND.
Note: all quotes in the COMMAND string will be escaped.
To say something, use:  (do-applescript \"say \\\"Hello\\\"\")"
        (shell-command
         (format
          "osascript -e \"%s\""
          (replace-regexp-in-string "\"" "\\\\\"" command)))))
  (defun do-applescript (_command)
    "No COMMAND executed, error raised: this requires macOS."
    (error "The do-applescript is only available on macOS systems!")))

;;-pel-autoload
(defun pel-say (text &optional translations)
  "Say TEXT out-loud.
Use the Apple osascript to say the text.
Quotes are not allowed inside the text.
Furthermore the optional TRANSLATIONS can be used
to exclude text from the narration.
This is a list of (`input regexp` . `output text`)
used to transform the text prior to narration.
Return t if the text was said, nil otherwise."
  (interactive "MSay: ")
  ;; Filter comments of text in current mode.
  (dolist (string (list comment-start
                        comment-end
                        comment-continue) text)
    (when string
      (setq text (replace-regexp-in-string (regexp-quote string) "" text))))
  ;; Filter using requested translations.
  (when translations
    (dolist (rule translations text)
      (setq text
            (replace-regexp-in-string
             (car rule)
             (cdr rule)
             text))))
  ;;
  (unless (string-match-p "\"" text)
    (if (fboundp 'do-applescript)
        (do-applescript
         (format "say \"%s\"%s"
                 text
                 (if (and (stringp pel-mac-voice-name)
                          (> (length pel-mac-voice-name) 2))
                     (format " using \"%s\"" pel-mac-voice-name)
                   "")))
      (user-error "The function do-applescript is not defined!"))
    t))

;;-pel-autoload
(defun pel-say-word ()
  "Say word at point out-loud and move to next word."
  (interactive)
  (pel-say (pel-word-at-point) pel-narration-translations))

;;-pel-autoload
(defun pel-say-sentence ()
  "Say sentence at point out-loud and move to next sentence."
  (interactive)
  (pel-say (pel-sentence-at-point) pel-narration-translations))

;;-pel-autoload
(defun pel-say-paragraph ()
  "Say paragraph at point out-loud and move to next paragraph."
  (interactive)
  (pel-say (pel-paragraph-at-point) pel-narration-translations))

;;-pel-autoload
(defun pel-say-region (start end)
  "Say text between region's START end END out-loud."
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring start end)))
      (pel-say text pel-narration-translations))))

;; --

(defun pel--convert-to-string (elems)
  "Convert ELEMS, a list of strings, characters and numbers to one string."
  (mapconcat
   (lambda (elem)
     (cond ((stringp elem)
            elem)
           ((and (integerp elem) (> elem 31) (< elem 127))
            (char-to-string elem))
           (t (format "%s" elem))))
   elems
   " "))

;;-pel-autoload
(defun pel-say-words (&rest words)
  "Say all WORDS."
  (pel-say (pel--convert-to-string words)))

;;-pel-autoload
(defun pel-run-applescript (programfile &rest args)
  "Run the AppleScript program identified by its PROGRAMFILE and its ARGS."
  ;; Issue a osascript progname args...
  (if pel-system-is-macos-p
      (let ((cmd (format "osascript %s" programfile)))
        (when args
          (setq cmd (format "%s %s"
                            cmd
                            (mapconcat
                             (lambda (arg) (format "%s" arg))
                             args
                             " "))))
        (shell-command cmd))
    (error "This is only available in macOS systems!")))

;; -----------------------------------------------------------------------------
(provide 'pel-applescript)

;;; pel-applescript.el ends here

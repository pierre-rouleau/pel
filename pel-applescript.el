;;; pel-applescript.el --- PEL AppleScript support -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2024, 2026  Pierre Rouleau

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
;; The `do-applescript' function is implemented invoking the osascript in a
;; child process instead of using the Cocoa built-in library.
;;
;; Currently the main purpose of this is to allow the use of the vocal
;; interface and read text out-loud.
;;
;; * `pel-say-word'
;; * `pel-say-sentence'
;; * `pel-say-paragraph'
;; * `pel-say-region'
;;   * `pel-say'
;;     - `do-applescript'
;; - `pel-say-words'
;;   - `pel--convert-to-strings'
;; - `pel-run-applescript'
;;
;; It also provides extra commands to compile .applescript files:

;; ---------------------------------------------------------------------------
;;; Code:

(require 'pel--base)                ; use: `pel-system-is-macos-p'
(require 'pel--options)
(require 'pel-read)                 ; use: `pel-mac-voice-name'

;; ---------------------------------------------------------------------------
;; Dictation Support
;; -----------------

(defconst pel-narration-translations
  '(("[_(){}`*~\\<>/^•]" . " ")
    ("\""  .  "")
    (";"   . ".  ")             ; pause longer on semicolon
    ("--+" . "")                ; remove  underlining
    ("==+" . "")                ; remove underlining
    ("#"   . " pound "))        ; only supporting English (todo: support more)

  "List of (`input regexp` . `output text`).
Used by `pel-say' to help translate input text to help smooth the narration.
The translation identified in the first list element is done first.")


;; --

;; forward definition to prevent byte-compiler warnings
(defvar pel-mac-voice-name)

(defun do-applescript (command)
  "Execute a small AppleScript COMMAND on macOS systems only.

To say something, use:  (do-applescript \"say \\\"Hello\\\"\")"
  (if pel-system-is-macos-p
      (progn
        (when (display-graphic-p)
          (require 'term/ns-win))
        (call-process "osascript" nil 0 nil "-e" command))
    (error "do-applescript is only available on macOS systems!")))

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
  (unless pel-system-is-macos-p
    (user-error "This is only available on macOS!"))
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
        (let ((cmd (format "say \"%s\"%s"
                           text
                           (if (and (boundp 'pel-mac-voice-name)
                                    (stringp pel-mac-voice-name)
                                    (> (length pel-mac-voice-name) 2))
                               (format " using \"%s\"" pel-mac-voice-name)
                             ""))))
          (message cmd)
          (do-applescript cmd))
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

;; ---------------------------------------------------------------------------
;; Applescript Compilation Support
;; -------------------------------

(defun pel-executable-found (executables)
  "Check for the presence of EXECUTABLES issue an error if any is missing.
This can be one string or a list of strings.
Return t if all are present."
  (dolist (executable (pel-list-of executables))
    (unless (executable-find executable)
      (user-error "%s not found!" executable)))
  ;; Return t
  t)

(defun pel-valid-macos-bundle-id-p (string &optional single-part-p)
  "Return t if STRING is valid for a macOS bundle ID configuration.
If SINGLE-PART-P is non-nil, checks if STRING is a valid individual component
\(alphanumeric and internal hyphens only, no periods).
If SINGLE-PART-P is nil or omitted, checks if STRING is a complete
3-component bundle ID separated by periods."
  (and (stringp string)
       (let* ((chunk-regex "[a-zA-Z0-9]+\\(?:-[a-zA-Z0-9]+\\)*")
              (complete-regex
               (if single-part-p
                   ;; only one chuck
                   (concat "\\`" chunk-regex "\\'")
                 ;; 3 dot-separated chunks only
                 (concat "\\`\\(?:" chunk-regex "\\.\\)\\{2\\}"
                         chunk-regex "\\'"))))
         (string-match-p complete-regex string))
       t)) ; Coerce the regex match position integer to a strict boolean t

(defun pel-prompt-for-macos-bundle-id ()
  "Interactively prompt the user for a valid 3-component macOS bundle ID.
Continuously re-prompts until the input is valid."
  (interactive)
  (let ((prompt "Enter macOS Bundle ID (e.g., com.example.app): ")
        input)

    ;; Loop until the user provides a string that our validator accepts
    (while (not (pel-valid-macos-bundle-id-p input))
      (setq input (read-string prompt))
      ;; Clean up any accidental leading/trailing whitespace typed by the user
      (setq input (string-trim input))
      ;; Update the prompt string if they fail the validation
      (setq prompt "\
Invalid format! Enter a valid 3-part Bundle ID (e.g., com.company.app): "))

    ;; Print a confirmation message to the echo area and return the clean string
    (message "Selected Bundle ID: %s" input)
    input))

(defun pel-can-build-macOS-bundle-for (fpath)
  "Return t if a valid macOS bundle ID can be inferred from FPATH.

If an ID made of the concatenation of \"com.<(basename FPATH)>.<REAL-USER-NAME>\"
is a macOS compliant ID return t, return nil otherwise."
  (and (pel-valid-macos-bundle-id-p
        (file-name-sans-extension
         (file-name-nondirectory
          (expand-file-name fpath)))
        'part)
       (pel-valid-macos-bundle-id-p (user-real-login-name) 'part)))

(defvar pel-home-dirpath-name)          ; defined in init.el

;;-pel-autoload
(defun pel-applescript-compile (&optional arg)
  "Compile the applescript file in buffer.

With no argument: perform static analysis.
With a single `\\[universal-argument]', compile to a .scpt script file.
With a double `\\[universal-argument]', compile to a .app application file.

When compiling, the generated file is stored in the same directory as the
source file.

When compiling to a bundled .app file, the bundle is signed with a macOS
compliant bundle ID.  No verification of valid bundle ID is done when
the command perform a static analysis or generates a .scpt file.

According to Apple's developer documentation, a bundle ID must follow a
uniform reverse-DNS structure where each individual component can only contain
alphanumeric characters (A-Z, a-z, 0-9) and hyphens (-). Crucially, a hyphen
cannot be the first or last character of a component. It can only be used
internally to separate words (e.g., com.my-company.my-app).

If a valid macOS bundle can generated automatically by using the basename of
the script file and the real user ID, then the command generates a bundle ID
this way, otherwise it prompts for a bundle ID."
  (interactive "P")
  (let ((fpath (buffer-file-name)))
    (if (not fpath)
        (user-error "Buffer is not visiting a file!")
      (let ((app-fpath (concat (file-name-sans-extension fpath) ".app")))
        (if (equal arg '(16))
            ;; Build signed .app file
            (let ((build-script (format "%s/bin/build-applescript"
                                        pel-home-dirpath-name)))
              ;; Check presence of build-applescript and the tools it uses.
              (when (pel-executable-found `("osacompile"
                                            "plutil"
                                            "tccutil"
                                            "codesign"
                                            ,build-script))
                ;; Can't build over existing .app; request its removal if it
                ;; already exist.
                (when (file-exists-p app-fpath)
                  (if (y-or-n-p (format "Delete existing %s" app-fpath))
                      (delete-directory app-fpath t)
                    (user-error "%s exists. Delete it first." app-fpath)))
                ;; Check that we can build a valid macOS bundle ID.
                ;; Use auto-generate ID if we can, otherwise prompt user for a
                ;; valid bundle ID.
                (if (pel-can-build-macOS-bundle-for fpath)
                    (compile (format "%s %s"
                                     (shell-quote-argument build-script)
                                     (shell-quote-argument fpath)))
                  (let ((bundle-id (pel-prompt-for-macos-bundle-id)))
                    (compile (format "%s %s --bundle-id=%s"
                                     (shell-quote-argument build-script)
                                     (shell-quote-argument fpath)
                                     bundle-id))))))
          ;; static compiling or building a .scpt file
          (when (pel-executable-found "osacompile")
            (let* ((no-ext-fname (file-name-sans-extension fpath))
                   (output-fname
                    (cond
                     ((null arg) "/dev/null")
                     ((equal arg '(4)) (shell-quote-argument
                                        (concat no-ext-fname ".scpt")))
                     (t (user-error "Invalid argument: %S" arg)))))
              (compile (format "osacompile -o %s %s" output-fname fpath)))))))
      ))

;; -----------------------------------------------------------------------------
(provide 'pel-applescript)

;;; pel-applescript.el ends here

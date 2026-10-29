;;; pel-emacs.el --- Emacs stats utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021, 2022, 2024, 2025, 2026  Pierre Rouleau

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; A set of of utilities related to Emacs itself.
;;
;; - `pel-emacs-executable' identifies the Emacs executable path and prints it
;;    on the echo area.
;; - `pel-open-emacs-refcard' prompts for the name of an Emacs reference card
;;   and opens the PDF file if it can locate it.  It attempts to locate the
;;   directory unless the `pel-emacs-refcard-dirpath' user option identify the
;;   directory where the reference card files are stored.
;;   - It uses the `pel-emacs-refcard-dirpath' utility to identify the
;;     location of the directory and topic specific PDF files.
;; - `pel-emacs-load-stats' print Emacs Lisp load statistics in the echo area.
;; - `pel-emacs-mem-stats' print Emacs Lisp memory statistics in a buffer.
;;   Those might be useful when developing Emacs Lisp code.
;;
;; The function call hierarchy:
;;
;; * `pel-emacs-executable'
;; * `pel-open-emacs-refcard'
;;   - `pel-emacs-refcard-dirpath'
;; * `pel-emacs-load-stats'
;; * `pel-emacs-mem-stats'
;; * `pel-emacs-command-stats'
;;   - `pel--emacs-command-count'
;;   - `pel--emacs-command-binding'
;; * `pel-emacs-compilation-features'
;; * `pel-emacs-bug-info'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)                    ; use: `pel-print-in-buffer',
                                        ;      `pel-emacs-27-or-later-p',
                                        ;      `pel-key-binding-string'

(require 'pel--options)
(require 'pel-prompt)                   ; use: `pel-prompt'
(require 'pel-setup-base)               ; use: `pel-startup-mode', `pel-with-package-quickstart-p'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-emacs-executable ()
  "Display Emacs executable path in echo area."
  (interactive)
  (message "Emacs := %s"
           (file-truename
            (expand-file-name invocation-name invocation-directory))))

;; ---------------------------------------------------------------------------
;; pel-open-emacs-refcard

(defvar pel--prompt-history-for-emacs-refcard nil
  "History list for function `pel-open-emacs-refcard'.")

(defun pel-emacs-refcard-dirpath (&optional topic)
  "Compute and return the path of Emacs refcard directory.
If TOPIC is non-nil, return the full path  of the specified topic PDF file."
  (let ((refcard-dirpath (or pel-emacs-refcard-dirpath
                             (expand-file-name
                              (format "../../share/emacs/%s/etc/refcards"
                                      emacs-version)
                              (file-truename
                               (expand-file-name
                                invocation-name
                                invocation-directory))))))
    (unless (file-exists-p refcard-dirpath)
      (user-error
       (if pel-emacs-refcard-dirpath
           "Directory identified by pel-emacs-refcard-dirpath is invalid!\
 Please fix it!"
         "Cannot locate Emacs refcard directory!
Please identify a directory with PDF refcard files in the \
pel-emacs-refcard-dirpath user option!")))
    (if topic
        (expand-file-name (format "%s.pdf" topic) refcard-dirpath)
      refcard-dirpath)))

;;-pel-autoload
(defun pel-open-emacs-refcard ()
  "Prompt for an Emacs REFCARD and open it.

Attempts to find the directory where the Emacs PDF reference card
files are stored.  Failing to detect them it uses the directory identified by
the variable `pel-emacs-refcard-dirpath' user option."
  (interactive)
  (let* ((topics (mapcar
                  (lambda (fn)
                    (substring fn 0 -4))
                  (directory-files
                   (pel-emacs-refcard-dirpath) nil  "\\.pdf\\'")))
         (topic  (completing-read
                  "Refcard: "
                  topics
                  nil                   ; predicate
                  t                     ; require-match
                  nil                   ; initial
                  'pel--prompt-history-for-emacs-refcard)))
    (browse-url (format "file:%s" (pel-emacs-refcard-dirpath topic)))))

;; ---------------------------------------------------------------------------
;; pel-emacs-load-stats

;;-pel-autoload
(defun pel-emacs-load-stats (&optional with-details)
  "Display number of loaded files & features.

If WITH-DETAILS, print details in the *emacs-load-stats* buffer.  With
\\[universal-argument] \\[universal-argument] prefix, also print content of `load-history'."
  (interactive "P")
  (if (and (require 'package nil :noerror)
           (boundp  'package-selected-packages)
           (boundp  'package-alist)
           (boundp  'package-activated-list))
      (let ((numeric-arg   (prefix-numeric-value with-details))
            (overview-msg
             (format "\
Emacs %-4s startup time: %s   (in %s mode%s)
# loaded files         : %d
# load-path length     : %d
# features             : %d
# package-alist        : %d
# packages activated   : %d
# packages selected    : %d"
                     emacs-version
                     (emacs-init-time)
                     (pel-startup-mode)
                     (if pel-emacs-27-or-later-p
                         (format
                          ", with%s package quickstart, with%s native compilation, %s."
                          (if (pel-with-package-quickstart-p)
                              ""
                            "out")
                          (if (and pel-emacs-28-or-later-p (featurep
                                                            'native-compile))
                              ""
                            "out")
                          (if (display-graphic-p)
                              (format "in graphics mode%s"
                                      (if (bound-and-true-p
                                           pel-init-support-dual-environment-p)
                                          " with dual environment"
                                        ""))
                            "in terminal mode"))
                       "")
                     (length load-history)
                     (length load-path)
                     (length features)
                     (length package-alist)
                     (length package-activated-list)
                     (length package-selected-packages))))
        (if with-details
            (pel-print-in-buffer
             "*emacs-load-stats*"
             "Emacs Load Statistics"
             (lambda ()
               "Print stats in buffer."
               (insert overview-msg
                       "\n\nfeatures, package-activated-list and package-selected-packages:\n")
               (when (>= numeric-arg 16)
                 (pel-insert-list-content 'load-history))
               (pel-insert-list-content 'features nil :without-index nil
                                        :on-same-line)
               (insert "\n")
               (pel-insert-list-content 'package-activated-list nil
                                        :without-index nil :on-same-line)
               (insert "\n")
               (pel-insert-list-content 'package-selected-packages nil
                                        :without-index nil :on-same-line)
               (toggle-truncate-lines -1)))
          (message overview-msg)))
    (user-error "The package file is not loaded!")))

;; ---------------------------------------------------------------------------
;; pel-emacs-mem-stats

;; Prevent byte-compiler warning by declaring variables that are
;; always available.
(defvar cons-cell-consed)
(defvar float-consed)
(defvar vector-cells-consed)
(defvar string-chars-consed)
;; (defvar misc-objects-consed)
(defvar intervals-consed)
(defvar strings-consed)

;;-pel-autoload
(defun pel-emacs-mem-stats ()
  "Display Emacs memory statistics inside an *emacs-mem-stats* buffer."
  (interactive)
  (pel-print-in-buffer "*emacs-mem-stats*"
                       "Emacs Memory"
                       (format "\
On %s:
 - cons-cells-consed  : %19d
 - floats-consed      : %19d
 - vector-cells-consed: %19d
 - symbols-consed     : %19d
 - string-chars-consed: %19d
 - intervals-consed   : %19d
 - strings-consed     : %19d%s"
                               (format-time-string "%A, %B %d, %Y @ %T")
                               cons-cells-consed
                               floats-consed
                               vector-cells-consed
                               symbols-consed
                               string-chars-consed
                               intervals-consed
                               strings-consed
                               (if (boundp 'misc-objects-consed)
                                   (format "
 - misc-objects-consed: %19d" misc-objects-consed)
                                 ""))))

;; ---------------------------------------------------------------------------
;; pel-emacs-command-stats

(defun pel--emacs-command-binding (command)
  "Return the first key bound to the COMMAND.

COMMAND must be a symbol.
Return a key binding string if there is a binding.
Return nil if there is no binding."
  (let ((key-description (pel-key-binding-string command))
        (m-x-sequence    (format "M-x %s" command)))
    (unless (string= key-description m-x-sequence)
      key-description)))

(defun pel--emacs-command-count (&optional predicate)
  "Return number of available commands that meet the (optional PREDICATE)."
  (let ((command-count 0))
  (mapatoms
   (lambda (symbol)
     (when (and (commandp symbol)
                (or (null predicate)
                    (funcall predicate symbol)))
       (pel+= command-count 1))))
  command-count))

;;-pel-autoload
(defun pel-emacs-command-stats ()
  "Display number of available commands and command with key bindings."
  (interactive)
  (message "\
Number of currently available commands    : %4d
Number of those commands with key bindings: %4d
Number of global keys (and key prefixes)  : %4d"
           (pel--emacs-command-count)
           (pel--emacs-command-count (function pel--emacs-command-binding))
           (length global-map)))

;; ---------------------------------------------------------------------------

(defun pel-emacs-buffer-stats ()
  "Print number of buffers buy types and last buffer."
  (interactive)
  (let ((special nil)
        (file-visiting nil)
        (internal nil))
    (dolist (bufname (mapcar (function buffer-name) (buffer-list)))
      (cond
       ((pel-starts-with-space-p bufname) (push bufname internal))
       ((pel-string-starts-with-p bufname "*")   (push bufname special))
       (t (push bufname file-visiting))))
    (message
     "%d buffers: %d visiting file, %d special, %d internal. Previous was: %S"
     (length (buffer-list))
     (length file-visiting)
     (length special)
     (length internal)
     (other-buffer (current-buffer)))))

(defun pel-emacs-config-features ()
  "Print the names of all Emacs configured compilation features."
  (interactive)
  (message (pel-emacs-config-features-string)))

(defun pel-emacs-bug-info (&optional in-browser)
  "Open the report about an Emacs bug report email stream.
Open it in Gnus buffer unless IN-BROWSER set to open in system browser."
  (interactive "P")
  (let ((url (format "https://%sdebbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                     ;; To force using system browser add extra // to make it
                     ;; fail the regexp search while being valid for system browsers
                     (if in-browser "//" "")
                     (pel-prompt "Bug number"))))
    (require 'debbugs-browse)
    (if (or in-browser
            (not (fboundp 'debbugs-browse-url)))
        (browse-url url)
      (declare-function debbugs-browse-url "debugs-browse")
      (debbugs-browse-url url))))

;; ---------------------------------------------------------------------------

(defun pel-daemon-client-count (gui-p)
  "Return the number of connected frame-associated clients.

If GUI-P is non-nil, count client processes associated with graphical
frames.  Otherwise, count client processes associated with text-terminal
frames.

Inspect the server in this Emacs instance.  Count each live client
connection once, even when several frames share that connection.

Exclude connections without an associated frame.  Also exclude
frames whose `client' parameter is `nowait', because that value
does not identify a client connection."
  (require 'server)
  (let (clients)
    (dolist (frame (frame-list) (length clients))
      (let ((client (frame-parameter frame 'client)))
        (when (and (if gui-p
                       (display-graphic-p frame)
                     (eq (framep frame) t))
                   (processp client)
                   (process-live-p client)
                   (boundp 'server-clients)
                   (memq client server-clients)
                   (not (memq client clients)))
          (push client clients))))))

(defun pel-daemon-graphical-frame-count ()
  "Return the number of graphical frames in this Emacs instance.
Include the selected graphical frame.
Do not require a live emacsclient connection.

Count all graphical frames, including frames created from Lisp
and child frames.  This is not a client-process count."
  (let ((count 0))
    (dolist (frame (frame-list) count)
      (when (display-graphic-p frame)
        (setq count (1+ count))))))


(defun pel-emacs-version-info (&optional append)
  "Display information about currently running Emacs."
  (interactive "P")
  (pel-print-in-buffer
   "*pel-session-info*"
   "PEL setup for Emacs Session Management"
   (lambda ()
     "Print Emacs session management status & configuration."
     (pel-insert-bold "Emacs Process\n")
     (pel-insert-bold "=============")
     (pel-insert-symbol-content-line 'emacs-version)
     (pel-insert-symbol-content-line 'user-emacs-directory)
     (pel-insert-symbol-content-line 'system-configuration)
     (pel-insert-value-info "Emacs executable" (expand-file-name
                                                invocation-name
                                                invocation-directory))
     (let ((cmdline (getenv "PEL_EMACS_LAUNCHER_COMMAND")))
       (when (and (stringp cmdline)
                  (not (string-empty-p cmdline)))
         (pel-insert-value-info "PEL Emacs startup command line" (string-trim
                                                                  cmdline))))
     ;; Query the process command line if possible.

     (let ((cmdline
            (if pel-system-is-windows-p
                ;; On Windows, use Powershell
                (condition-case nil
                    (when-let* ((powershell (executable-find "powershell.exe")))
                      (let* ((coding-system-for-read 'utf-8)
                             (script
                              (format
                               (concat
                                "$ErrorActionPreference = 'Stop'; "
                                "[Console]::OutputEncoding = "
                                "[System.Text.UTF8Encoding]::new($false); "
                                "(Get-CimInstance -ClassName Win32_Process "
                                "-Filter 'ProcessId = %d' "
                                "-ErrorAction Stop).CommandLine")
                               (emacs-pid)))
                             (output
                              (string-trim
                               (mapconcat
                                #'identity
                                (process-lines powershell
                                               "-NoLogo" "-NoProfile" "-NonInteractive"
                                               "-Command" script)
                                "\n"))))
                        (unless (string-empty-p output)
                          output)))
                  (error nil))
              ;; On Darwin, Linux, Unix: use ps
              (condition-case nil
                  (when (executable-find "ps")
                    (car (process-lines "ps" "-o" "args=" "-p"
                                        (number-to-string (emacs-pid)))))
                (error nil)))))
       (pel-insert-value-info
        "Real Emacs command line used"
        (if (and cmdline
                 (not (string-empty-p (string-trim cmdline))))
            (string-trim cmdline)
          "Unavailable (ps missing, failed, or returned no command line)")))
     ;;
     (pel-insert-value-info "Emacs daemon client?"
                            (pel-yes-no-string (daemonp)))
     ;;
     (when (daemonp)
       (pel-insert-value-info "Active terminal client process count"
                              (pel-daemon-client-count nil))
       (pel-insert-value-info "Active graphical client process count"
                              (pel-daemon-client-count t))
       (pel-insert-value-info "All active graphical client frame count"
                              (pel-daemon-graphical-frame-count))
       (when (boundp 'server-name)
         (pel-insert-value-info "\
Notes:
  - A GUI client normally does not create a persisting client process.
    It will, however, create a GUI frame.
  - The counts refer to the currently used Emacs daemon socket" server-name))
       (let ((cmdline (getenv "PEL_EMACS_LAST_CLIENT_COMMAND")))
         (when (and (stringp cmdline)
                    (not (string-empty-p cmdline)))
           (pel-insert-value-info "Latest PEL client command line"
                                  (string-trim cmdline))
           (pel-insert-symbol-content-line 'server-name))))

     (pel-insert-value-info "Process ID" (number-to-string (emacs-pid)))
     (pel-insert-value-info "Configuration features"
                            (pel-emacs-config-features-string))
     (when (and (fboundp 'treesit-available-p)
                (boundp 'treesit-extra-load-path))
       (insert "\n\n")
       (pel-insert-bold "Tree-Sitter\n")
       (pel-insert-bold "===========")
       (pel-insert-value-info "TreeSitter available" (pel-yes-no-string
                                                      (treesit-available-p)))
       (pel-insert-list-content 'treesit-extra-load-path)))
   (unless append :clear-buffer)
   :use-help-mode))

;;; --------------------------------------------------------------------------
(provide 'pel-emacs)

;;; pel-emacs.el ends here

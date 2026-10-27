;;; pel-compile.el --- PEL compilation helper  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-27 09:02:56 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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

;; @ `pel-with-controlled-save'
;;    - `pel--execute-smart-save-pipeline'
;;      - `pel--compile-handle-current-buffer-save'
;;      - `pel-compile-get-buffers-needing-save'
;;         - `pel--save-buffer-for-compile-p'
;;           - `pel--compile-mode-match-p'
;;      - `pel--compile-prompt-save-intent'
;;      - `pel--compile-display-ibuffer-menu'
;;      - `pel--save-some-buffers-for-compile'
;;
;;    o `pel--compile-save-workspace-mode'
;;      - `pel--compile-ibuffer-execute-and-build'
;;      - `pel--compile-ibuffer-vcs-diff'
;;      * `pel-compile-info'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)      ; use: `pel-mode-name-for',
;;                        ;      `pel-major-modes-for'
(require 'pel--options)   ; use: `pel-compile-save-mode-scope',
;;                        ;      `pel-compile-related-modes-alist',
;;                        ;      `pel-compile-batch-save-threshold',
(require 'ibuffer)        ; use: `ibuffer-map-lines'
(require 'ibuf-ext)       ; use: `ibuffer-filter-groups'
(require 'vc)             ; use: `vc-diff-internal'
(require 'compile)        ; use: `compilation-save-buffers-predicate'
(require 'cl-lib)         ; use: `cl-letf'
(require 'ibuffer)        ; use: `ibuffer-map-lines'
;;                        ;      `ibuffer-current-buffer'
;;                        ;      `ibuffer-mark-interactive'
;;                        ;      `ibuffer-diff-with-file'
;;                        ;      `ibuffer-filter-groups'
;;                        ;      `ibuffer-use-header-line',
;;                        ;      `ibuffer-header-line-format'
;;                        ;      `ibuffer-show-empty-filter-groups'
;;                        ;      `ibuffer-update', `ibuffer-set-mark'

(defvar ibuffer-header-line-format)     ; declare to prevent Emacs 26 warnings
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--compile-origin-buffer nil
  "Internal variable to track the buffer that initiated a PEL compilation.")

(defun pel--compile-mode-match-p (origin-mode target-mode scope)
  "Return non-nil if TARGET-MODE matches ORIGIN-MODE under SCOPE.

ORIGIN-MODE and TARGET-MODE are major mode symbols ending in
\"-mode\".

If SCOPE is nil or `all', match every mode.
If SCOPE is `none', match no mode.

If SCOPE is `same-mode', match modes with the same base name.
Also match TARGET-MODE when it derives from either the classic
or Tree-sitter variant of ORIGIN-MODE's base name.

If SCOPE is `related', also match TARGET-MODE when its base name
appears in the entry for ORIGIN-MODE's base name in
`pel-compile-related-modes-alist'.  These explicit relationships
are directional.

Signal an error for any other SCOPE."
  (cond
   ((or (null scope) (eq scope 'all)) t)
   ((eq scope 'none) nil)
   ((memq scope '(same-mode related))
    (let* ((origin-name (pel-mode-name-for origin-mode))
           (target-name (pel-mode-name-for target-mode))
           (same-mode-p
            (or (equal origin-name target-name)
                (cl-some
                 (lambda (mode)
                   (provided-mode-derived-p target-mode mode))
                 (pel-major-modes-for origin-name)))))
      (or same-mode-p
          (and (eq scope 'related)
               (member target-name
                       (cdr (assoc origin-name
                                   pel-compile-related-modes-alist)))))))
   (t
    (error "Invalid scope: %S" scope))))

(defun pel--save-buffer-for-compile-p ()
  "Return non-nil if current buffer must  be saved before `compile'.

This is predicate for `save-some-buffers' to implement PEL compilation
buffer save logic."
  (let ((target-buffer (current-buffer)))
    (and (buffer-file-name target-buffer)
         (buffer-live-p pel--compile-origin-buffer)
         ;; The current-buffer handler already processed the origin.
         ;; Do not prompt for it again or save it automatically here.
         (not (eq target-buffer pel--compile-origin-buffer))
         (let ((origin-mode (buffer-local-value 'major-mode
                                                pel--compile-origin-buffer))
               (target-mode major-mode))
           (pel--compile-mode-match-p origin-mode
                                      target-mode
                                      pel-compile-save-mode-scope)))))

;; ---------------------------------------------------------------------------
(defvar-local pel-compile--batch-callback nil
  "Compilation callback pending in this save workspace, or nil.")

;; Ibuffer reconstructs marks from displayed rows. A hidden buffer can
;; therefore lose its mark during a later refresh.
;;
;; Use a workspace-local alist as the persistent mark store. Before each
;; refresh, copy displayed marks into that store. Then supply the stored marks
;; when Ibuffer builds its new list.
;;
;; The advice below is registered globally, but its behavior is restricted to
;; an active PEL save workspace. Normal Ibuffer buffers call the original
;; function without changes

(defvar-local pel-compile--save-marks nil
  "Alist of original candidate buffers and their workspace marks.
Each entry has the form (BUFFER . MARK).  Hidden buffers retain
their last mark.  Only the S mark selects a buffer for saving.")

(defun pel--compile-ibuffer-sync-save-marks ()
  "Copy displayed candidate marks into this workspace's mark store."
  (dolist (entry (ibuffer-current-state-list))
    (let ((saved-entry (assq (car entry) pel-compile--save-marks)))
      (when saved-entry
        (setcdr saved-entry (cdr entry))))))

(defun pel--compile-ibuffer-preserve-save-marks (original buffers)
  "Call ORIGINAL for BUFFERS, preserving PEL workspace candidate marks."
  (if (not (and (derived-mode-p 'ibuffer-mode)
                pel-compile--batch-callback
                pel-compile--save-marks))
      (funcall original buffers)
    ;; Capture edits before Ibuffer filters or regenerates its rows.
    (pel--compile-ibuffer-sync-save-marks)
    (mapcar
     (lambda (entry)
       (let ((saved-entry (assq (car entry) pel-compile--save-marks)))
         (if saved-entry
             (cons (car entry) (cdr saved-entry))
           entry)))
     (funcall original buffers))))

(advice-add 'ibuffer-current-buffers-with-marks :around
            #'pel--compile-ibuffer-preserve-save-marks)


(defun pel--compile-ibuffer-unmark-all ()
  "Clear all marks in this compilation save workspace.

Clear displayed Ibuffer marks and stored marks for all original
candidates, including candidates hidden by filters or collapsed
filter groups.  Do not save buffers or start compilation."
  (interactive)
  ;; Clear displayed marks first.  This command also redisplays Ibuffer,
  ;; which can synchronize displayed marks into the persistent store.
  (ibuffer-unmark-all-marks)
  ;; Clear the store after redisplay so hidden marks cannot survive.
  ;; Retain each entry: it identifies an original save candidate.
  (dolist (entry pel-compile--save-marks)
    (setcdr entry ?\s))
  (message "PEL: All save selections cleared, including hidden buffers."))

(defun pel--compile-ibuffer-execute-and-build ()
  "Save the files explicitly marked with S in the workspace, then compile.

Save selected original candidates even when filters hide their rows.
If no candidates have an S mark, save nothing and compile."
  (interactive)
  (let ((callback pel-compile--batch-callback))
    (unless callback
      (user-error "No compilation is pending in this workspace"))
    ;;
    ;; Capture the latest displayed marks, including explicit unmarks.
    (pel--compile-ibuffer-sync-save-marks)
    ;; Keep the callback available if a save fails or the user quits.
    ;; Use a snapshot because save hooks can update the workspace.
    (dolist (entry (copy-tree pel-compile--save-marks))
      (let ((buf (car entry))
            (mark (cdr entry)))
        (when (and (buffer-live-p buf) (eq mark ?S))
          (with-current-buffer buf
            (save-buffer)
            (when (buffer-modified-p)
              ;; If a buffer save fails warn user
              (user-error "Buffer %s is still modified; compilation not started"
                          (buffer-name)))))))
    ;;
    ;; All selected saves succeeded.  Consume this workspace's callback.
    (setq pel-compile--batch-callback nil)
    (kill-buffer (current-buffer))
    ;;
    ;; Start compilation without further compilation save prompting.
    (when callback
      (let ((compilation-save-buffers-predicate #'ignore))
        (funcall callback)))))

(defun pel--compile-ibuffer-vcs-diff ()
  "Perform a VCS-specific diff on the buffer at point if file tracked by VCS."
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
    (if (and buf (buffer-file-name buf))
        (let ((backend (vc-backend (buffer-file-name buf))))
          (if backend
              (vc-diff-internal nil
                                (list backend
                                      (list (buffer-file-name buf)))
                                nil nil)
            (message "Buffer file is not registered under version control.")))
      (message "No file associated with this buffer."))))

(defun pel--compile-ibuffer-abort ()
  "Abort this workspace's pending compilation without saving buffers."
  (interactive)
  (setq pel-compile--batch-callback nil)
  (kill-buffer (current-buffer))
  (message "PEL: Compilation aborted."))

(defun pel--compile-ibuffer-mark-for-save (arg)
  "Mark ARG buffers for saving, or the filter group at point."
  (interactive "p")
  (ibuffer-mark-interactive arg ?S))

;; ---------------------------------------------------------------------------
;; pel--compile-save-workspace-mode Minor Mode
;; ------------------------------------------

(defun pel-compile-info ()
  "Describe the specialized ibuffer prompt to save buffers before compile."
  (interactive)
  (pel-print-in-buffer
   "*pel-compile-info*"
   "Using the buffer saving specialized ibuffer"
   (lambda ()
     "Describe the purpose of the specialized ibuffer."
     (insert "\n")
     (pel-insert-bold "Identify Buffer to save before compilation\n")
     (pel-insert-bold "==========================================\n\n")
     (insert "\
The specialized ibuffer uses the following minor mode:\n- ")
     (pel-insert-symbol 'pel--compile-save-workspace-mode)
     (insert "\n\n\
This buffer allows you to quickly identify the buffers that must be
saved before proceeding with the compilation you just requested.  PEL
has identified the buffers that may need to be saved before proceeding
with the compilation command and shows them in this specialized ibuffer.
You can remove some of them here or change the way PEL identifies them.

Inside the specialized ibuffer, each buffer marked with a 'S' will be saved
before proceeding with the compilation command.

Inside that buffer:

- unmark a buffer by typing ")
     (pel-insert-bold "u")
     (insert ",
- unmark all buffers, including hidden candidates, by typing ")
     (pel-insert-bold "U")
     (insert ",
- proceed with the compilation by typing ")
     (pel-insert-bold "x")
     (insert ",
- abort (don't compile) by typing ")
     (pel-insert-bold "q")
     (insert ".

Before proceeding or aborting you can also quickly check for the changes
between the buffer content and its file content by typing ")
     (pel-insert-bold "d")
     (insert ",
or between the file content itself and the currently committed
content of the file by typing ")
     (pel-insert-bold "D")
     (insert " if that file is under VCS control.\n
Note that you can also use the standard iBuffer filtering commands to
filter a long list of buffer to help you identify the buffers that
should be unmarked or marked back.\n\n")
     (pel-insert-bold "Customize PEL behaviour\n")
     (pel-insert-bold "=======================\n\n")
     (insert "\
The way PEL identifies which buffer may need to be saved and how PEL prompts
you is controlled by the following user-options:\n")
     (pel-insert-symbol-content-line 'pel-compile-save-mode-scope)
     (pel-insert-symbol-content-line 'pel-compile-batch-save-threshold)
     (pel-insert-symbol-content-line 'pel-compile-skip-prompt-to-save-current-buffer)
     (insert "\n\n\
With pel-compile-save-mode-scope set to 'related, PEL checks if unsaved buffers
using related major modes may also need to be saved.  PEL uses the mode association
identified in the following association list to do that:\n ")
     (pel-insert-list-content        'pel-compile-related-modes-alist)
     (insert "\n\n\
In the above, the first element of the list is the mode base name of a buffer
to compile, and the others are the base name of major modes of buffers that are
identified as relevant."))
   :clear-buffer
   :use-help-mode))


(defvar pel--compile-save-workspace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'pel-compile-info)
    (define-key map (kbd "D") #'pel--compile-ibuffer-vcs-diff)
    (define-key map (kbd "d") #'ibuffer-diff-with-file)
    (define-key map (kbd "m") #'pel--compile-ibuffer-mark-for-save)
    (define-key map (kbd "q") #'pel--compile-ibuffer-abort)
    (define-key map (kbd "U") #'pel--compile-ibuffer-unmark-all)
    (define-key map (kbd "x") #'pel--compile-ibuffer-execute-and-build)
    map)
  "Keymap for `pel--compile-save-workspace-mode'.")

(defconst pel--compile-save-workspace-mode-help
  (concat "Save before compile? | "
          "m: save | u/U: no save/all| "
          "d: Δbuf/file | D: Δfile/VC | "
          "x: compile | q: abort | ?: help | (FILTERS active)")
  "A single help line shown at the top of the specialized ibuffer.")

(define-minor-mode pel--compile-save-workspace-mode
  "Review buffer saves before compilation in a PEL Ibuffer workspace.

PEL enables this buffer-local minor mode in each compilation save
workspace.  It supplements `ibuffer-mode'.

Initially, each listed modified buffer has an S mark for saving.
Unmark buffers that you do not want to save.

Filtering changes which rows are displayed, not the save selection.
Hidden candidates retain their marks.  The save-and-compile command
also saves hidden candidates that retain an S mark.
Only buffers selected when this workspace opened can be saved by
that command.

\\<pel--compile-save-workspace-mode-map>\
\\[pel--compile-ibuffer-mark-for-save] marks buffers with S for saving.
The standard Ibuffer key `u' removes a mark.
\\[pel--compile-ibuffer-unmark-all] removes all marks, including marks on
hidden candidates.
\\[ibuffer-diff-with-file] compares the buffer with its saved file.
\\[pel--compile-ibuffer-vcs-diff] compares the saved file with its VC revision.
The VC comparison does not include unsaved buffer edits.

\\[pel--compile-ibuffer-execute-and-build] saves S-marked buffers, closes this
workspace, and starts compilation.  With no S marks, it saves nothing.
\\[pel--compile-ibuffer-abort] aborts this workspace's compilation and closes
the workspace without saving buffers.
\\[describe-mode] displays mode help, including these instructions.

Disabling this minor mode removes its key bindings.  It does not
abort the pending compilation or remove the workspace header.

\\{pel--compile-save-workspace-mode-map}"
  :init-value nil
  :lighter " PEL-Save"
  :keymap pel--compile-save-workspace-mode-map
  :group 'pel-compile)

;; ---------------------------------------------------------------------------

(defun pel-compile-get-buffers-needing-save ()
  "Collect all file-visiting modified buffers matching the save mode criteria."
  (let (collected)
    (dolist (buf (buffer-list))
      (when (and (buffer-modified-p buf)
                 (buffer-file-name buf))
        (with-current-buffer buf
          (when (pel--save-buffer-for-compile-p)
            (push buf collected)))))
    collected))

(defun pel--compile-display-ibuffer-menu (buffers-to-save callback)
  "Open a new save workspace for BUFFERS-TO-SAVE and CALLBACK.

Keep CALLBACK local to the workspace.  Each workspace has its own
buffer-selection filter and does not replace another pending request."
  (let ((origin-mode (with-current-buffer pel--compile-origin-buffer major-mode))
        (buf-name (generate-new-buffer-name "*PEL Compile Save Workspace*")))
    (ibuffer t buf-name
             `((predicate . (memq (current-buffer) ',buffers-to-save))))
    (with-current-buffer (get-buffer buf-name)
      ;; Set request state after Ibuffer initializes its major mode.
      (setq-local pel-compile--batch-callback callback)
      (pel--compile-save-workspace-mode 1)
      ;; Install this once per workspace.  Ibuffer refreshes copy this
      ;; format into `header-line-format' while filters are active.
      ;; Keep the standard filter information after the workspace help.
      (setq-local ibuffer-use-header-line t)
      (setq-local ibuffer-header-line-format
                  pel--compile-save-workspace-mode-help)
      (setq-local ibuffer-filter-groups
            `(("Compilation Mode Targets" (mode . ,origin-mode))
              ("Secondary Cluster Targets" (used-mode t))))
      (setq-local ibuffer-show-empty-filter-groups nil)
      (ibuffer-update nil t)
      ;; Pre-mark every buffer for saving automatically
      (goto-char (point-min))
      (while (not (eobp))
        (let ((buf (ibuffer-current-buffer)))
          (when (and buf (buffer-modified-p buf))
            ;; Mark the buffer for save
            (ibuffer-set-mark ?S)))
        (forward-line 1))
      ;; Remember all buffers initially marked:
      ;; initialize after the first display and marking pass.  Include
      ;; candidates hidden by Ibuffer display settings or filter groups.
      (setq-local pel-compile--save-marks
                  (mapcar (lambda (buf) (cons buf ?S))
                          buffers-to-save))
      (goto-char (point-min)))
    (message "\
PEL: [m] Mark Save | [u/U] Unmark | [d] Diff | [D] VCS Diff | [x] Save & Compile | [?] help")))

(defun pel--compile-prompt-save-intent (count)
  "Prompt the user how to handle COUNT modified buffers before compiling.

Returns a symbol representing the chosen path: \\='ibuffer,
\\='sequential, \\='skip, or \\='abort."
  (let ((prompt (format "\
PEL: %d modified buffers found.
[i] Show in Ibuffer, [s] Sequential prompt, [k] Skip saves, [q] Quit/Abort: " count))
        (choices '(?i ?s ?k ?q)))
    (pcase (read-char-choice prompt choices)
      (?i 'ibuffer)
      (?s 'sequential)
      (?k 'skip)
      (?q 'abort))))

(defun pel--compile-handle-current-buffer-save ()
  "Process the current buffer save policy before compilation.

Return non-nil after saving the current buffer or skipping its save.
Declining the save does not cancel compilation.
Quit and save errors propagate to the caller."
  (if (and (buffer-file-name)
           (buffer-modified-p))

      (if pel-compile-skip-prompt-to-save-current-buffer
          ;; save modified buffer right away; no prompt.
          (progn
            (save-buffer)
            t)
        ;; A negative answer skips this save, not compilation.
        (when (y-or-n-p (format "Save modified buffer '%s' before compiling? "
                               (buffer-name)))
          (save-buffer))
        t)
    t)) ; Buffer already saved: proceed to compilation.



 (defun pel--save-some-buffers-for-compile ()
   "Save in-scope buffers without bypassing the compilation predicate."
   (let (automatic-save-values)
     ;; - Analyze buffers, identify buffers with auto-save needed.
     ;;   Each entry of automatic-save-values list has:
     ;;   - buffer
     ;;   - `buffer-save-without-query' value
     ;;   - `buffer-save-without-query': t if local variable, nil otherwise
     ;; - For buffers that must absolutely be saved without query set the
     ;;   buffer local value of their `buffer-save-without-query' to nil.
     (dolist (buffer (buffer-list))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and buffer-save-without-query
                      (not (pel--save-buffer-for-compile-p)))
             (push (list buffer
                         buffer-save-without-query
                         (local-variable-p 'buffer-save-without-query))
                   automatic-save-values)
             (setq-local buffer-save-without-query nil)))))
     ;; Perform the save some buffer operation
     (unwind-protect
         (save-some-buffers nil #'pel--save-buffer-for-compile-p)
       ;; Restore the state of `buffer-save-without-query' of the affected
       ;; buffers and for those that were not using a local value, remove the
       ;; local copy for the buffer.
       (dolist (entry automatic-save-values)
         (when (buffer-live-p (car entry))
           (with-current-buffer (car entry)
             (if (caddr entry)
                 (setq-local buffer-save-without-query (cadr entry))
               (kill-local-variable 'buffer-save-without-query))))))))

(defun pel--execute-smart-save-pipeline (compile-callback)
  "Evaluates buffer save counts; executes the user's explicit interface route."
  ;; 1. Check current buffer saving policies first.
  ;;    A declined save skips the current buffer.  Quit and save errors
  ;;    stop the pipeline.
  (when (pel--compile-handle-current-buffer-save)
    ;;
    ;; 2. Gather any OTHER modified buffers matching mode filtering rules
    (let* ((all-targets (pel-compile-get-buffers-needing-save))
           ;; Explicitly exclude current buffer; it was already handled above
           (buffers-to-save (delq (current-buffer) all-targets))
           (count (length buffers-to-save)))

      (cond
       ;; Case 0: No sibling modified buffers match -> compile instantly
       ((zerop count)
        (funcall compile-callback))

       ;; Case 1: Sibling threshold reached -> deploy intent choices menu
       ((>= count pel-compile-batch-save-threshold)
        (pcase (pel--compile-prompt-save-intent count)
          ('ibuffer
           (pel--compile-display-ibuffer-menu buffers-to-save
                                              compile-callback))

          ('sequential
           ;; Proceed: prompt saving only for relevant files, then compile.
           (let ((compilation-ask-about-save nil))
             (pel--save-some-buffers-for-compile)
             (funcall compile-callback)))

          ('skip
           ;; Skip saving files, then compile.
           (let ((compilation-save-buffers-predicate #'ignore))
             (funcall compile-callback)))

          ('abort
           (message "PEL: Compilation cancelled."))))

       ;; Case 2: Under threshold -> standard filtered sequential prompts
       ;;         for siblings
       (t
        (let ((compilation-ask-about-save nil))
          ;; Proceed to prompt for saving only the relevant buffers.
          (pel--save-some-buffers-for-compile)
          ;; Then compile, skip saving buffers the user decided not to save or
          ;; buffers that are not relevant.
          (let ((compilation-save-buffers-predicate #'ignore))
            (funcall compile-callback))))))))

;; ---------------------------------------------------------------------------

(defmacro pel-with-controlled-save (&rest body)
  "Safely execute BODY after prompting or batching mode-relevant buffer saves.

Disable `save-some-buffers' while BODY runs so compilation cannot save
buffers skipped by the save pipeline.  This also applies when the save
scope is `none'.

Use this macro as wrapper to the `compilation' calls as in the following
code:

 (pel-with-controlled-save
    ;; Your existing command execution code here
    (compile \"sphinx-build -b html ...\"))

CAUTION: When the save pipeline opens an Ibuffer save workspace, BODY
execution is deferred until the user completes the workspace save
operation.  Other save paths can execute BODY during this macro call.
A non-`none' value of `pel-compile-save-mode-scope' does not by itself
cause deferral.

Place all setup needed to start compilation inside BODY.  This includes
temporary bindings and cleanup handlers that must remain active when
compilation starts."
  (declare (indent 0) (debug t))
  `(let* ((original-buffer (current-buffer))
          (pel--compile-origin-buffer original-buffer)
          (compilation-save-buffers-predicate #'ignore))
     (if (eq pel-compile-save-mode-scope 'none)
         ;; When operation must not prompt and must not save any buffer:
         ;; the predicate alone cannot suppress unconditional saves made
         ;; for buffers with non-nil `buffer-save-without-query'.
         (cl-letf (((symbol-function 'save-some-buffers) #'ignore))
           ,@body)
       ;;
       (pel--execute-smart-save-pipeline
        (lambda ()
          (with-current-buffer original-buffer
            ;; Enter this binding when the callback runs, including when
            ;; Ibuffer invokes it after the save pipeline has returned.
            (cl-letf (((symbol-function 'save-some-buffers) #'ignore))
              ,@body)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-compile)

;;; pel-compile.el ends here

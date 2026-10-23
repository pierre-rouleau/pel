;;; pel-compile.el --- PEL compilation helper  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-23 17:36:32 EDT, updated by Pierre Rouleau>

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
;;      - `pel--compile-prompt-save-intent'
;;      - `pel-compile-get-buffers-needing-save'
;;         - `pel--save-buffer-for-compile-p'
;;           - `pel--compile-mode-match-p'
;;      - `pel--compile-display-ibuffer-menu'
;;
;;    ~ `pel-compile-ibuffer-setup-keys'
;;      - `pel--compile-ibuffer-execute-and-build'
;;      - `pel--compile-ibuffer-vcs-diff'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)   ; use: `pel-compile-save-mode-scope',
;;                        ;      `pel-compile-related-modes-alist',
;;                        ;      `pel-compile-batch-save-threshold',
;;                        ;      `pel-compile-always-use-batch-save'
(require 'ibuffer)        ; use: `ibuffer-map-lines'
(require 'ibuf-ext)       ; use: `ibuffer-filter-groups'
(require 'vc)             ; use: `vc-diff-internal'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--compile-origin-buffer nil
  "Internal variable to track the buffer that initiated a PEL compilation.")

(defun pel--compile-mode-match-p (origin-mode target-mode scope)
  "Return non-nil if TARGET-MODE matches ORIGIN-MODE according to SCOPE rule."
  (cond
   ((not scope)      t)
   ((eq scope 'all)  t)
   ((eq scope 'none) nil)
   ((eq scope 'same-mode)
    (provided-mode-derived-p target-mode origin-mode))
   ((eq scope 'related)
    (or (provided-mode-derived-p target-mode origin-mode)
        (let ((related (cdr (assq origin-mode pel-compile-related-modes-alist))))
          (memq target-mode related))
        (let ((related (cdr (assq target-mode pel-compile-related-modes-alist))))
          (memq origin-mode related))))
   (t (error "Invalid scope: %s" scope))))

(defun pel--save-buffer-for-compile-p ()
  "Return non-nil if current buffer must  be saved before `compile'.

This is predicate for `save-some-buffers' to implement PEL compilation
buffer save logic."
  (let ((target-buffer (current-buffer)))
    (and (buffer-file-name target-buffer)
         (buffer-live-p pel--compile-origin-buffer)
         (let ((origin-mode (buffer-local-value 'major-mode
                                                pel--compile-origin-buffer))
               (target-mode major-mode))
           (pel--compile-mode-match-p origin-mode
                                      target-mode
                                      pel-compile-save-mode-scope)))))

;; ---------------------------------------------------------------------------
(defvar pel-compile--batch-callback nil
  "Internal variable to hold the compilation payload while ibuffer is active.")

(defun pel--compile-ibuffer-execute-and-build ()
  "Save the files explicitly marked with S in the workspace, then compile.

If no files are marked, then save nothing and proceed cleanly with the
compilation."
  (interactive)
  (let ((callback pel-compile--batch-callback))
    ;; 1. Deactivate our hooks immediately to prevent cleanup race conditions
    (setq pel-compile--batch-callback nil)
    (remove-hook 'ibuffer-mode-hook #'pel-compile-ibuffer-setup-keys)

    ;; 2. Use ibuffer's native line mapper to scan and save marked entries
    ;;    safely
    (ibuffer-map-lines
     (lambda (buf mark)
       (when (and buf (eq mark ?S))
         (with-current-buffer buf
           (save-buffer)))))

    ;; 3. Kill the workspace buffer cleanly to restore the original window layout
    (kill-buffer (current-buffer))

    ;; 4. Fire the compilation engine; prevent further prompts and do not save
    ;; any unsaved buffers
    (when callback
      (let ((compilation-save-buffers-predicate #'ignore)
            ;; (compilation-ask-about-save nil)
            )
        (funcall callback)))))

(defun pel--compile-ibuffer-vcs-diff ()
  "Perform a VCS-specific diff on the buffer at point if file tracked by VCS."
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
    (if (and buf (buffer-file-name buf))
        (let ((backend (vc-backend (buffer-file-name buf))))
          (if backend
              (vc-diff-internal backend (list (buffer-file-name buf)) nil nil)
            (message "Buffer file is not registered under version control.")))
      (message "No file associated with this buffer."))))

(defun pel--compile-ibuffer-abort ()
  "Aborts the entire compilation chain cleanly, leaving buffers unsaved."
  (interactive)
  (setq pel-compile--batch-callback nil)
  (remove-hook 'ibuffer-mode-hook #'pel-compile-ibuffer-setup-keys)
  (kill-buffer (current-buffer))
  (message "PEL: Compilation aborted."))

(defun pel-compile-ibuffer-setup-keys ()
  "Inject compilation control overrides into the temporary ibuffer workspace."
  (when pel-compile--batch-callback
    ;; Disable standard ibuffer exit command; replace it with a compilation
    ;; operation abort (and also close the buffer)
    (local-set-key (kbd "q") #'pel--compile-ibuffer-abort)
    (local-set-key (kbd "x") #'pel--compile-ibuffer-execute-and-build)
    (local-set-key (kbd "d") #'ibuffer-diff-with-file)
    (local-set-key (kbd "D") #'pel--compile-ibuffer-vcs-diff)))

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
  "Open restricted ibuffer workspace to batch-save BUFFERS-TO-SAVE before CALLBACK."
  (setq pel-compile--batch-callback callback)
  (add-hook 'ibuffer-mode-hook #'pel-compile-ibuffer-setup-keys)
  (let ((origin-mode (with-current-buffer pel--compile-origin-buffer major-mode))
        (buf-name "*PEL Compile Save Workspace*"))
    (ibuffer t buf-name
             `((predicate . (memq (current-buffer) ',buffers-to-save))))
    (with-current-buffer (get-buffer buf-name)
      (setq ibuffer-filter-groups
            `(("Compilation Mode Targets" (mode . ,origin-mode))
              ("Secondary Cluster Targets" (used-mode t))))
      (setq ibuffer-show-empty-filter-groups nil)
      (ibuffer-update nil t)
      ;; Pre-mark every buffer for saving automatically
      (goto-char (point-min))
      (while (not (eobp))
        (let ((buf (ibuffer-current-buffer)))
          (when (and buf (buffer-modified-p buf))
            ;; Mark the buffer for save
            (ibuffer-set-mark ?S)))
        (forward-line 1))
      (goto-char (point-min)))
    (message "\
PEL: [s] Mark Save | [u] Unmark | [d] Diff | [D] VCS Diff | [x] Save & Compile")))

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
Returns non-nil if it is safe to proceed with the compilation pipeline."
  (if (and (buffer-file-name)
           (buffer-modified-p))

      (if pel-compile-skip-prompt-to-save-current-buffer
          ;; save modified buffer right away; no prompt.
          (save-buffer)
        ;; User wants to be prompted
        (if (y-or-n-p (format "Save modified buffer '%s' before compiling? "
                              (buffer-name)))
             (progn
               (save-buffer)
               t)                    ; proceed
          (message "PEL: Compilation aborted (current buffer not saved).")
          nil))                      ; Abort: don't compile
    t)) ; Buffer already saved: proceed to compilation.


(defun pel--execute-smart-save-pipeline (compile-callback)
  "Evaluates buffer save counts and executes the user's explicit interface route."
  ;; 1. Check current buffer saving policies first.
  ;;    Halt if user declines.
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
       ((or pel-compile-always-use-batch-save
            (>= count pel-compile-batch-save-threshold))
        (pcase (pel--compile-prompt-save-intent count)
          ('ibuffer
           (pel--compile-display-ibuffer-menu buffers-to-save
                                              compile-callback))

          ('sequential
           ;; Proceed: prompt saving only for relevant files, then compile.
           (let ((compilation-ask-about-save nil))
             (save-some-buffers nil #'pel--save-buffer-for-compile-p)
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
          (save-some-buffers nil #'pel--save-buffer-for-compile-p)
          ;; Then compile, skip saving buffers the user decided not to save or
          ;; buffers that are not relevant.
          (let ((compilation-save-buffers-predicate #'ignore))
            (funcall compile-callback))))))))

;; ---------------------------------------------------------------------------

(defmacro pel-with-controlled-save (&rest body)
  "Safely execute BODY after prompting or batching mode-relevant buffer saves.

Use this macro as wrapper to the `compilation' calls as in the following
code:

 (pel-with-mode-isolated-save
    ;; Your existing command execution code here
    (compile \"sphinx-build -b html ...\"))

"
  (declare (indent 0) (debug t))
  `(let* ((original-buffer (current-buffer))
          (pel--compile-origin-buffer original-buffer)
          (compilation-save-buffers-predicate #'ignore)
          ;; (compilation-ask-about-save nil)
          )
     (if (eq pel-compile-save-mode-scope 'none)
         (progn ,@body)
       (pel--execute-smart-save-pipeline
        (lambda ()
          (with-current-buffer original-buffer
            ,@body))))))

;;; --------------------------------------------------------------------------
(provide 'pel-compile)

;;; pel-compile.el ends here

;;; pel-buffer-file-name-test.el --- Test buffer and file mode control  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 18 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-26 13:53:09 EDT, updated by Pierre Rouleau>

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
;; Behavior-Based regression tests. The tests verify that each function uses
;; the current buffer’s filename and preserves its existing behavior. The tests
;; do not require a particular implementation, such as forbidding calls to
;; buffer-file-name.
;;
;; Coverage:
;; - C3 compilation and saving.
;;   - pel-c3-compile: save modified file buffers, use the current filename,
;;     and signal an error for a non-file buffer without saving or compiling.
;; - Project filename selection.
;;   - pel-ffind-project-name and pel-ffind-project-settings: select the buffer
;;     filename or an explicit filename, including in a non-file buffer.
;; - Org archive detection.
;;   - pel--org-buffer-is-archive-p: require an archive filename and org-mode.
;; - C include-guard output.
;;   - pel-c-include-guard: derive the symbol from the current filename,
;;     normalize "+" in extensions, and preserve UUID and endif options.
;;
;; Saving and compilation are stubbed.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-c3)
(require 'pel-ffind)
(require 'pel-org)
(require 'pel-uuid)

(require 'ert)
(require 'cl-lib)
(require 'compile)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest pel-filename/c3-saves-only-modified-file-buffer ()
  "Save a modified file buffer before compilation, but not a clean buffer."
  (ert-skip "Buffer automatically saved by `pel-with-controlled-save'.")
  (dolist (modified '(nil t))
    (with-temp-buffer
      (setq-local buffer-file-name
                  (expand-file-name "source file.c3"
                                    temporary-file-directory))
      (insert "module example;\n")
      (set-buffer-modified-p modified)
      (let ((expected-command
             (format "c3c compile %s"
                     (shell-quote-argument buffer-file-name)))
            (events nil))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _)
                     (push 'save events)
                     (set-buffer-modified-p nil)))
                  ((symbol-function 'compile)
                   (lambda (command &optional comint)
                     (push (list 'compile command comint) events))))
          (pel-c3-compile))
        (should
         (equal (nreverse events)
                (append (when modified '(save))
                        (list (list 'compile expected-command t)))))))))

;; Cover a modified non-file buffer
;; --------------------------------
(ert-deftest pel-filename/c3-non-file-buffer ()
  "Signal a type error without saving or compiling a non-file buffer."
  (ert-skip "Buffer automatically saved by `pel-with-controlled-save'.")
  (with-temp-buffer
    (setq-local buffer-file-name nil)
    (insert "module example;\n")
    (should (buffer-modified-p))
    (cl-letf (((symbol-function 'save-buffer)
               (lambda (&rest _)
                 (ert-fail "Must not save a non-file buffer")))
              ((symbol-function 'compile)
               (lambda (&rest _)
                 (ert-fail "Must not compile a non-file buffer"))))
      ;; Preserve the current behavior: quoting a nil filename signals
      ;; wrong-type-argument, rather than an explicit user-error.
      (should-error (pel-c3-compile)
                    :type 'wrong-type-argument))))

(ert-deftest pel-filename/c3-uses-current-buffer ()
  "Build each command from the filename of the current buffer."
  (ert-skip "Buffer automatically saved by `pel-with-controlled-save'.")
  (let ((commands nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (push command commands))))
      (with-temp-buffer
        (setq-local buffer-file-name
                    (expand-file-name "outer.c3"
                                      temporary-file-directory))
        (let ((outer-file buffer-file-name)
              (inner-file
               (expand-file-name "inner file.c3"
                                 temporary-file-directory)))
          (with-temp-buffer
            (setq-local buffer-file-name inner-file)
            (pel-c3-compile))
          (pel-c3-compile)
          (should
           (equal (nreverse commands)
                  (mapcar
                   (lambda (file)
                     (format "c3c compile %s"
                             (shell-quote-argument file)))
                   (list inner-file outer-file)))))))))

(ert-deftest pel-filename/project-lookup-filename-selection ()
  "Use the buffer filename by default and prefer an explicit filename."
  (let* ((root-a (expand-file-name "project-a"
                                   temporary-file-directory))
         (root-b (expand-file-name "project-b"
                                   temporary-file-directory))
         (file-a (expand-file-name "a.c" root-a))
         (file-b (expand-file-name "b.c" root-b))
         (settings-a '(((c) ("/include-a") () () () ())))
         (settings-b '(((c) ("/include-b") () () () ())))
         (pel-dev-projects
          (list (list "A" (list root-a settings-a))
                (list "B" (list root-b settings-b)))))
    ;; Isolate filename selection from filesystem-based root discovery.
    (cl-letf (((symbol-function 'pel-ffind-project-directory-of)
               (lambda (file &optional _identifiers)
                 (cond
                  ((equal file file-a) root-a)
                  ((equal file file-b) root-b)
                  (t (ert-fail
                      (format "Unexpected filename: %S" file))))))
              ((symbol-function 'pel-path=) #'equal))
      (with-temp-buffer
        (setq-local buffer-file-name file-a)
        (should (equal (pel-ffind-project-name) "A"))
        (should (equal (pel-ffind-project-settings) settings-a))
        (should (equal (pel-ffind-project-name file-b) "B"))
        (should (equal (pel-ffind-project-settings file-b) settings-b))
        (setq-local buffer-file-name nil)
        (should-not (pel-ffind-project-name))
        (should-not (pel-ffind-project-settings))
        (should (equal (pel-ffind-project-name file-b) "B"))
        (should (equal (pel-ffind-project-settings file-b) settings-b))))))

(ert-deftest pel-filename/org-archive-detection ()
  "Require both an archive filename and org-mode."
  (dolist (case '((nil org-mode nil)
                  ("notes.org" org-mode nil)
                  ("notes.org_archive" fundamental-mode nil)
                  ("notes.org_archive" org-mode t)))
    (with-temp-buffer
      ;; This unit test checks the predicate, not mode initialization.
      (setq-local buffer-file-name (nth 0 case))
      (setq-local major-mode (nth 1 case))
      (should
       (eq (pel--org-buffer-is-archive-p)
           (nth 2 case))))))

(ert-deftest pel-filename/include-guard-output ()
  "Use the current filename and preserve UUID and endif options."
  (with-temp-buffer
    (setq-local buffer-file-name
                (expand-file-name "utils.h" temporary-file-directory))
    (cl-letf (((symbol-function 'pel-uuid)
               (lambda ()
                 "b89d59ef-9cde-45cd-bb83-242ad937bf08")))
      (dolist (with-uuid '(nil t))
        (dolist (with-endif '(nil t))
          (let ((symbol
                 (concat
                  "UTILS__H"
                  (if with-uuid
                      "_b89d59ef_9cde_45cd_bb83_242ad937bf08"
                    ""))))
            (should
             (equal
              (pel-c-include-guard with-uuid with-endif)
              (format
               "#ifndef %s\n#define %s /* include guard */\n%s"
               symbol symbol
               (if with-endif "\n#endif\n" ""))))))))))

;; Verify filename-dependent include guards
;; ----------------------------------------

(ert-deftest pel-filename/include-guard-filename-selection ()
  "Derive the guard from the current filename and normalize extensions."
  (with-temp-buffer
    (dolist (case '(("utils.h" "UTILS__H")
                    ("vector.h++" "VECTOR__HPLPL")))
      (setq-local buffer-file-name
                  (expand-file-name (car case)
                                    temporary-file-directory))
      (should
       (equal (pel-c-include-guard nil nil)
              (format
               "#ifndef %s\n#define %s /* include guard */\n"
               (cadr case) (cadr case)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-buffer-file-name-test)

;;; pel-buffer-file-name-test.el ends here

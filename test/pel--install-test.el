;;; pel--install-test.el --- Test installatoin helper logic  -*- lexical-binding: t; -*-

;; Created   : Wednesday, October 14 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-18 11:51:47 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--install)
(require 'ert)
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Test the distinction between top-level and nested alternation
;; =============================================================

(ert-deftest pel-regexp-top-level-alternation ()
  "A top-level alternation needs protection before adding an anchor."
  (should
   (pel--regexp-top-level-alternation-p "icc\\|inl"))

  ;; A nested alternation does not require another enclosing group.
  (should-not
   (pel--regexp-top-level-alternation-p
    "\\.\\(?:icc\\|inl\\)"))

  ;; An escaped literal backslash followed by | is not alternation.
  (should-not
   (pel--regexp-top-level-alternation-p "\\\\|"))

  ;; Backslash and | inside a character class are literal characters.
  (should-not
   (pel--regexp-top-level-alternation-p "[\\|]"))

  ;; This class also contains a literal closing bracket.
  (should-not
   (pel--regexp-top-level-alternation-p "[]\\|]")))


;; Test a known group reduction
;; ============================

(ert-deftest pel-regexp-removes-redundant-outer-group ()
  "Remove the redundant outer group without changing literal matches."
  (let* ((patterns '(".inl" ".icc"))
         (original (regexp-opt patterns))
         (optimized
          (pel--literal-alternatives-regexp patterns))
         (anchored (concat "\\`" optimized "\\'")))
    ;; The optimized regexp must not be longer than the original.
    (should (<= (length optimized) (length original)))
    (should (string-prefix-p "\\(?:" original))
    (should (string-suffix-p "\\)" original))
    (should (equal optimized (substring original 4 -2)))

    (dolist (name '(".icc" ".inl"))
      (should (string-match-p anchored name)))
    (dolist (name '(".iccx" ".inlx" "x.icc" ".cc" ".nl"))
      (should-not (string-match-p anchored name)))))


;; Test matching behaviour against an independent rule
;; ===================================================

(ert-deftest pel-regexp-filenames-and-suffixes ()
  "Test matching behaviour against an independent rule"
  (let ((regexp
         (pel--patterns-to-regexp
          '("Emakefile" "/.erlang" ".inl" ".icc"))))
    (dolist (filename '("/project/Emakefile"
                        "/project/.erlang"
                        "/project/header.inl"
                        "/project/header.icc"
                        "C:/project/Emakefile"))
      (should (string-match-p regexp filename)))

    (dolist (filename '("/project/MyEmakefile"
                        "/project/Emakefile.old"
                        "/project/other.erlang"
                        "/project/header.inlx"
                        "/project/headerXicc"
                        "/project/Emakefile/source.txt"))
      (should-not (string-match-p regexp filename)))))

;; ---------------------------------------------------------------------------
;; Test pel--auto-activate-fly
;; ===========================
;;
;; The following tests establish:
;;
;; - Both engines receive argument 1, not a toggle request.
;; - An exact filename does not match a filename with an added suffix.
;; - A directory matches nested files, but not a sibling with the same prefix.
;; - A pathname without a trailing / remains an exact-file entry.
;; - Overlapping matches cause only one activation call.
;; - Both matching branches stop before a later invalid entry.
;;
;; The early-exit test should fail against the previous full-scan
;; implementation. The other tests establish baseline behavior for later
;; changes.
;;
;; These are unit tests for pel--auto-activate-fly. They do not test hook timing
;; or interaction with an already active engine.
;;
;; The helper uses a temporary buffer. It does not create or visit files. It
;; replaces both engine entry points, so the tests do not load Flycheck or start
;; syntax checking.

(defun pel-test--auto-activate-fly-calls
    (filename pathnames engines &optional flycheck-enabled mode)
  "Return engine activation calls for the specified buffer settings.
FILENAME is relative to a synthetic `default-directory', or nil.
PATHNAMES and ENGINES supply the activation options.
FLYCHECK-ENABLED supplies `pel-use-flycheck'.
MODE defaults to `emacs-lisp-mode'."
  (with-temp-buffer
    (let ((default-directory
           (file-name-as-directory
            (expand-file-name "pel-auto-activate-fly-test"
                              temporary-file-directory)))
          (pel-files-activating-syntax-check pathnames)
          (pel-fly-engine-for-modes engines)
          (pel-use-flycheck flycheck-enabled)
          (calls nil))
      ;; Set buffer metadata without visiting a file or running mode hooks.
      (setq buffer-file-name
            (when filename
              (expand-file-name filename))
            major-mode (or mode 'emacs-lisp-mode))
      (cl-letf (((symbol-function 'flymake-mode)
                 (lambda (arg)
                   (push (list 'flymake arg) calls)))
                ((symbol-function 'flycheck-mode)
                 (lambda (arg)
                   (push (list 'flycheck arg) calls))))
        (pel--auto-activate-fly))
      (nreverse calls))))

(ert-deftest pel-auto-activate-fly-no-file ()
  "Do not activate an engine in a buffer without a file."
  ;; The invalid entry must not be examined.
  (should-not
   (pel-test--auto-activate-fly-calls
    nil '(42) '((emacs-lisp flymake)))))

(ert-deftest pel-auto-activate-fly-no-engine ()
  "Do not activate when the mode has no selected engine."
  (dolist (engines '(nil
                     ((emacs-lisp nil))
                     ((python flymake))))
    (ert-info ((format "Engine configuration: %S" engines))
      ;; The invalid entry must not be examined.
      (should-not
       (pel-test--auto-activate-fly-calls
        "app/main.el" '(42) engines)))))

(ert-deftest pel-auto-activate-fly-empty-pathnames ()
  "Do not activate when no pathname is configured."
  (should-not
   (pel-test--auto-activate-fly-calls
    "app/main.el" nil '((emacs-lisp flymake)))))

(ert-deftest pel-auto-activate-fly-pathname-matching ()
  "Preserve exact-file and recursive directory matching."
  (dolist (case '(("app/main.el" "app/main.el" t)
                  ("app/main.el.old" "app/main.el" nil)
                  ("app/main.el" "app/other.el" nil)
                  ("app/main.el" "app/" t)
                  ("app/src/main.el" "app/" t)
                  ("application/main.el" "app/" nil)
                  ("app/main.el" "app" nil)))
    (ert-info ((format "Matching case: %S" case))
      (let ((calls
             (pel-test--auto-activate-fly-calls
              (nth 0 case)
              (list (nth 1 case))
              '((emacs-lisp flymake)))))
        (should
         (equal calls
                (when (nth 2 case)
                  '((flymake 1)))))))))

(ert-deftest pel-auto-activate-fly-finds-later-match ()
  "Continue past non-matching entries."
  (should
   (equal
    (pel-test--auto-activate-fly-calls
     "app/main.el"
     '("elsewhere/" "app/other.el" "app/main.el")
     '((emacs-lisp flymake)))
    '((flymake 1)))))

(ert-deftest pel-auto-activate-fly-activates-once ()
  "Activate once when several configured entries match."
  (should
   (equal
    (pel-test--auto-activate-fly-calls
     "app/main.el"
     '("app/" "app/main.el" "app/")
     '((emacs-lisp flymake)))
    '((flymake 1)))))

(ert-deftest pel-auto-activate-fly-stops-after-first-match ()
  "Do not examine entries after an exact-file or directory match."
  (dolist (matching-path '("app/main.el" "app/"))
    (ert-info ((format "First matching pathname: %S" matching-path))
      ;; Expanding 42 would signal an error if scanning continued.
      (should
       (equal
        (pel-test--auto-activate-fly-calls
         "app/main.el"
         (list "elsewhere/" matching-path 42)
         '((emacs-lisp flymake)))
        '((flymake 1)))))))

(ert-deftest pel-auto-activate-fly-flycheck-enabled ()
  "Activate Flycheck, and not Flymake, when Flycheck is enabled."
  (should
   (equal
    (pel-test--auto-activate-fly-calls
     "app/main.el" '("app/") '((emacs-lisp flycheck)) t)
    '((flycheck 1)))))

(ert-deftest pel-auto-activate-fly-flycheck-disabled ()
  "Do not activate either engine when selected Flycheck is disabled."
  (should-not
   (pel-test--auto-activate-fly-calls
    "app/main.el" '("app/") '((emacs-lisp flycheck)) nil)))

(ert-deftest pel-auto-activate-fly-tree-sitter-mode ()
  "Use the base mode name to select a Tree-sitter mode's engine."
  (should
   (equal
    (pel-test--auto-activate-fly-calls
     "app/main.py" '("app/") '((python flymake)) nil
     'python-ts-mode)
    '((flymake 1)))))

;;; --------------------------------------------------------------------------
(provide 'pel--install-test)

;;; pel--install-test.el ends here

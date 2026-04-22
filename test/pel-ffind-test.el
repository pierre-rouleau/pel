;;; pel-ffind-test.el --- Test pel-ffind.el  -*- lexical-binding: t; -*-

;; Created   : Monday, April 20 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-20 14:49:47 EDT, updated by Pierre Rouleau>

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
;; ERT tests for the public API of pel-ffind.el.
;;
;; Coverage:
;;   Section 1 : pel-dev-project.*           -- structural field accessors
;;   Section 2 : pel-dev-project.setting.*   -- per-language setting accessors
;;   Section 3 : pel-ffind-env-tool-names    -- environment variable lookup
;;   Section 4 : pel--ffind-dirname-expanded -- path quoting/expansion helper
;;   Section 5 : pel-ffind-reset-cache       -- buffer-local cache clearing
;;   Section 6 : pel-ffind-project-directory-of -- anchor-file upward search
;;   Section 7 : pel-ffind-project-name      -- project name resolution
;;   Section 8 : pel-ffind-project-settings  -- project settings resolution
;;   Section 9 : pel-ffind-project-lang-*    -- language-specific accessors
;;   Section 10: pel-ffind (integration)     -- filesystem + find/fd
;;   Section 11: pel-generic-find-file       -- high-level search
;;
;; NOTE: Section 6 complements, but does not duplicate, the tests in
;;       test/pel-ffind-project-rootdir-test.el.
;;
;; Sections 10 and 11 require the `find' (or `fd') command and are skipped
;; automatically on systems where neither is available.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-ffind)
(require 'pel--options)   ; use: `pel-project-root-identifiers',
                          ;      `pel-project-restricted-root-identifiers',
                          ;      `pel-dev-projects', `pel-dev-tools',
                          ;      `pel-dev-libraries'
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ffind-test--touch (file)
  "Create an empty FILE, creating its parent directories as needed."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file (expand-file-name file) (insert "")))

(defmacro pel-ffind-test--with-temp-dir (dir-sym &rest body)
  "Bind DIR-SYM to a fresh temporary directory, then evaluate BODY.
The directory and all its contents are deleted on exit."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "pel-ffind-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir-sym t))))

(defmacro pel-ffind-test--with-env (var value &rest body)
  "Set environment variable VAR to VALUE around BODY, restoring on exit.
Pass nil for VALUE to unset the variable."
  (declare (indent 2))
  `(let ((--saved-- (getenv ,var)))
     (unwind-protect
         (progn (setenv ,var ,value) ,@body)
       (setenv ,var --saved--))))

(defmacro pel-ffind-test--with-global-tool-reset (&rest body)
  "Evaluate BODY with `pel--ffind-executable' and `pel--ffind-path' reset to nil.
Both globals are restored to their original values on exit."
  (declare (indent 0))
  `(let ((--saved-exe--  pel--ffind-executable)
         (--saved-path-- pel--ffind-path))
     (unwind-protect
         (progn
           (setq pel--ffind-executable nil
                 pel--ffind-path       nil)
           ,@body)
       (setq pel--ffind-executable --saved-exe--
             pel--ffind-path       --saved-path--))))

;;; --------------------------------------------------------------------------
;;; Section 1 – pel-dev-project.* structural field accessors
;;; --------------------------------------------------------------------------
;;
;; A pel-dev-projects entry has the shape:
;;   (name (root-dir (setting1 setting2 ...)))

(ert-deftest pel-dev-project-accessor/name ()
  "pel-dev-project.name returns the first element – the project name string."
  (should (equal (pel-dev-project.name '("myproject" ("/root" ())))
                 "myproject")))

(ert-deftest pel-dev-project-accessor/root-dir ()
  "pel-dev-project.root-dir returns the project root directory string."
  (should (equal (pel-dev-project.root-dir '("p" ("/path/to/root" ())))
                 "/path/to/root")))

(ert-deftest pel-dev-project-accessor/settings ()
  "pel-dev-project.settings returns the list of per-language setting plists."
  (let* ((s1  '((c) ("/tree1") () () () ()))
         (s2  '((c++) ("/tree2") () () () ()))
         (proj (list "p" (list "/root" (list s1 s2)))))
    (should (equal (pel-dev-project.settings proj) (list s1 s2)))))

(ert-deftest pel-dev-project-accessor/empty-settings ()
  "pel-dev-project.settings returns nil when no settings are configured."
  (should (null (pel-dev-project.settings '("p" ("/root" ()))))))

;;; --------------------------------------------------------------------------
;;; Section 2 – pel-dev-project.setting.* per-language setting accessors
;;; --------------------------------------------------------------------------
;;
;; A single setting (one element of the settings list) has 6 ordered slots:
;;   (languages dir-trees tools libs envvars exclude-regexps)

(defvar pel-ffind-test--full-setting
  '((c c++)                    ; 0 – languages
    ("/tree/a" "/tree/b")      ; 1 – dir-trees
    ("gcc" "clang")            ; 2 – tools
    ("boost" "gtest")          ; 3 – libs
    ("CPATH" "C_INCLUDE_PATH") ; 4 – envvars
    ("\\.o$" "\\.a$"))         ; 5 – exclude-regexps
  "A fully-populated setting list used by setting-accessor tests.")

(ert-deftest pel-dev-project-setting-accessor/languages ()
  "pel-dev-project.setting.languages returns the language symbol list."
  (should (equal (pel-dev-project.setting.languages pel-ffind-test--full-setting)
                 '(c c++))))

(ert-deftest pel-dev-project-setting-accessor/dir-trees ()
  "pel-dev-project.setting.dir-trees returns the directory-trees list."
  (should (equal (pel-dev-project.setting.dir-trees pel-ffind-test--full-setting)
                 '("/tree/a" "/tree/b"))))

(ert-deftest pel-dev-project-setting-accessor/tools ()
  "pel-dev-project.setting.tools returns the tool-name list."
  (should (equal (pel-dev-project.setting.tools pel-ffind-test--full-setting)
                 '("gcc" "clang"))))

(ert-deftest pel-dev-project-setting-accessor/libs ()
  "pel-dev-project.setting.libs returns the library-name list."
  (should (equal (pel-dev-project.setting.libs pel-ffind-test--full-setting)
                 '("boost" "gtest"))))

(ert-deftest pel-dev-project-setting-accessor/envvars ()
  "pel-dev-project.setting.envvars returns the environment-variable list."
  (should (equal (pel-dev-project.setting.envvars pel-ffind-test--full-setting)
                 '("CPATH" "C_INCLUDE_PATH"))))

(ert-deftest pel-dev-project-setting-accessor/exclude-regexps ()
  "pel-dev-project.setting.exclude-regexps returns the exclusion regexp list."
  (should (equal (pel-dev-project.setting.exclude-regexps pel-ffind-test--full-setting)
                 '("\\.o$" "\\.a$"))))

(ert-deftest pel-dev-project-setting-accessor/nil-languages ()
  "A nil languages slot means the setting applies to all languages."
  (let ((setting '(nil ("/tree") () () () ())))
    (should (null (pel-dev-project.setting.languages setting)))))

(ert-deftest pel-dev-project-setting-accessor/empty-slots ()
  "All slots may be empty lists without signalling an error."
  (let ((setting '(() () () () () ())))
    (should (null (pel-dev-project.setting.languages setting)))
    (should (null (pel-dev-project.setting.dir-trees setting)))
    (should (null (pel-dev-project.setting.tools setting)))
    (should (null (pel-dev-project.setting.libs setting)))
    (should (null (pel-dev-project.setting.envvars setting)))
    (should (null (pel-dev-project.setting.exclude-regexps setting)))))

;;; --------------------------------------------------------------------------
;;; Section 3 – pel-ffind-env-tool-names
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-env-tool-names/unset-variable-returns-nil ()
  "Returns nil when PEL_DEV_TOOLS_FOR_LANG is not set."
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C" nil
    (should (null (pel-ffind-env-tool-names 'c)))))

(ert-deftest pel-ffind-env-tool-names/empty-string-returns-nil ()
  "Returns nil when the environment variable is set but empty."
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C" ""
    (should (null (pel-ffind-env-tool-names 'c)))))

(ert-deftest pel-ffind-env-tool-names/single-tool ()
  "Returns a one-element list when a single tool name is set."
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C" "gcc"
    (should (equal (pel-ffind-env-tool-names 'c) '("gcc")))))

(ert-deftest pel-ffind-env-tool-names/multiple-tools ()
  "Returns multiple tool names from a colon-separated variable value."
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C" "gcc:clang:arm-gcc"
    (should (equal (pel-ffind-env-tool-names 'c) '("gcc" "clang" "arm-gcc")))))

(ert-deftest pel-ffind-env-tool-names/cpp-maps-to-CPP-not-C++ ()
  "The c++ symbol maps to PEL_DEV_TOOLS_FOR_CPP, not PEL_DEV_TOOLS_FOR_C++."
  ;; Phase 1: c++ reads PEL_DEV_TOOLS_FOR_CPP — returns "g++" when CPP is set.
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_CPP" "g++"
    (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C++" nil
      (should (equal (pel-ffind-env-tool-names 'c++) '("g++")))))
  ;; Phase 2: PEL_DEV_TOOLS_FOR_C++ (literal) is NOT consulted —
  ;; returns nil even when C++ literal variable is set, because only CPP is used.
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_CPP" nil
    (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_C++" "g++"
      (should (null (pel-ffind-env-tool-names 'c++))))))

(ert-deftest pel-ffind-env-tool-names/uppercase-language-in-varname ()
  "The environment variable name uses UPPER-CASE language suffix."
  ;; 'perl maps to PEL_DEV_TOOLS_FOR_PERL, not PEL_DEV_TOOLS_FOR_perl
  (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_PERL" "my-perl-tool"
    (pel-ffind-test--with-env "PEL_DEV_TOOLS_FOR_perl" nil
      (should (equal (pel-ffind-env-tool-names 'perl) '("my-perl-tool"))))))

;;; --------------------------------------------------------------------------
;;; Section 4 – pel--ffind-dirname-expanded
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-dirname-expanded/absolute-path ()
  "An absolute path is wrapped in single quotes unchanged."
  (should (equal (pel--ffind-dirname-expanded "/usr/local/include")
                 "'/usr/local/include'")))

(ert-deftest pel-ffind-dirname-expanded/trailing-slash-removed ()
  "A trailing slash is stripped before quoting."
  (should (equal (pel--ffind-dirname-expanded "/usr/local/include/")
                 "'/usr/local/include'")))

(ert-deftest pel-ffind-dirname-expanded/tilde-expanded ()
  "A leading ~ is expanded to the home directory."
  (let* ((result   (pel--ffind-dirname-expanded "~"))
         (expected (format "'%s'"
                           (directory-file-name (expand-file-name "~")))))
    ;; Must not contain a literal tilde
    (should-not (string-match-p (regexp-quote "~") result))
    ;; Must equal the expanded home path in single quotes
    (should (equal result expected))))

(ert-deftest pel-ffind-dirname-expanded/env-var-expanded ()
  "A $VARNAME style environment variable in the path is expanded."
  (pel-ffind-test--with-env "PEL_TEST_INCDIR" "/opt/include"
    (should (equal (pel--ffind-dirname-expanded "$PEL_TEST_INCDIR")
                   "'/opt/include'"))))

(ert-deftest pel-ffind-dirname-expanded/result-starts-and-ends-with-single-quote ()
  "The result always starts and ends with a single-quote character."
  (let ((result (pel--ffind-dirname-expanded "/some/path")))
    (should (string-prefix-p "'" result))
    (should (string-suffix-p "'" result))))

;;; --------------------------------------------------------------------------
;;; Section 5 – pel-ffind-reset-cache
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-reset-cache/clears-all-buffer-local-vars ()
  "Resets all buffer-local cache variables to nil."
  (with-temp-buffer
    (setq pel--ffind-search-initialized t
          pel--ffind-searched-dirs      '("/a/dir")
          pel--ffind-searched-dir-trees '("/a/tree")
          pel--ffind-exclusion-regexps  '(".*\\.o$"))
    (pel-ffind-reset-cache nil 'silently)
    (should (null pel--ffind-search-initialized))
    (should (null pel--ffind-searched-dirs))
    (should (null pel--ffind-searched-dir-trees))
    (should (null pel--ffind-exclusion-regexps))))

(ert-deftest pel-ffind-reset-cache/does-not-clear-tool-by-default ()
  "Without clear-search-tool, global tool variables are left untouched."
  (pel-ffind-test--with-global-tool-reset
    (setq pel--ffind-executable 'fd
          pel--ffind-path       "/usr/bin/fdfind")
    (with-temp-buffer
      (pel-ffind-reset-cache nil 'silently)
      (should (eq   pel--ffind-executable 'fd))
      (should (equal pel--ffind-path      "/usr/bin/fdfind")))))

(ert-deftest pel-ffind-reset-cache/clears-tool-when-requested ()
  "With a non-nil CLEAR-SEARCH-TOOL argument, global tool variables are cleared."
  (pel-ffind-test--with-global-tool-reset
    (setq pel--ffind-executable 'fd
          pel--ffind-path       "/usr/bin/fdfind")
    (with-temp-buffer
      (pel-ffind-reset-cache t 'silently)
      (should (null pel--ffind-executable))
      (should (null pel--ffind-path)))))

(ert-deftest pel-ffind-reset-cache/idempotent-on-already-nil-cache ()
  "Calling reset-cache on an already-empty cache does not signal an error."
  (with-temp-buffer
    (should-not (eq (pel-ffind-reset-cache nil 'silently) 'error))))

;;; --------------------------------------------------------------------------
;;; Section 6 – pel-ffind-project-directory-of
;;; --------------------------------------------------------------------------
;;
;; These tests complement pel-ffind-project-rootdir-test.el by covering the
;; pel-ffind-project-directory-of function directly with an explicit PATHNAME
;; argument.

(ert-deftest pel-ffind-project-directory-of/outermost-normal-anchor-wins ()
  "Returns the outermost directory when multiple normal anchors exist."
  (pel-ffind-test--with-temp-dir root
    (let* ((file-deep (expand-file-name "a/b/c/deep.c" root))
           (pel-project-root-identifiers          '(".git" ".hg"))
           (pel-project-restricted-root-identifiers '()))
      (pel-ffind-test--touch file-deep)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (pel-ffind-test--touch (expand-file-name ".hg"
                                               (expand-file-name "a" root)))
      (should (equal (pel-ffind-project-directory-of file-deep)
                     (directory-file-name (expand-file-name root)))))))

(ert-deftest pel-ffind-project-directory-of/restricted-anchor-stops-search ()
  "A restricted anchor stops the upward walk even when outer normal anchors exist."
  (pel-ffind-test--with-temp-dir root
    (let* ((inner-dir  (expand-file-name "proj/inner" root))
           (deep-file  (expand-file-name "proj/inner/src/code.c" root))
           (pel-project-root-identifiers          '(".git"))
           (pel-project-restricted-root-identifiers '(".my-restricted")))
      (pel-ffind-test--touch deep-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))   ; outer
      (pel-ffind-test--touch (expand-file-name ".my-restricted" inner-dir)) ; nearer restricted
      (should (equal (pel-ffind-project-directory-of deep-file)
                     (directory-file-name (expand-file-name inner-dir)))))))

(ert-deftest pel-ffind-project-directory-of/no-anchors-returns-nil ()
  "Returns nil when no anchor files are present above PATHNAME."
  (pel-ffind-test--with-temp-dir root
    (let* ((file (expand-file-name "no-anchor/file.c" root))
           (pel-project-root-identifiers          '(".git" ".hg"))
           (pel-project-restricted-root-identifiers '()))
      (pel-ffind-test--touch file)
      (should (null (pel-ffind-project-directory-of file))))))

(ert-deftest pel-ffind-project-directory-of/extra-identifiers-extend-search ()
  "Identifiers passed as the second argument augment the configured set."
  (pel-ffind-test--with-temp-dir root
    (let* ((file (expand-file-name "sub/file.c" root))
           (pel-project-root-identifiers          '())     ; nothing configured
           (pel-project-restricted-root-identifiers '()))
      (pel-ffind-test--touch file)
      ;; Without any identifier, no root is found.
      (should (null (pel-ffind-project-directory-of file)))
      ;; Create a custom anchor at root.
      (pel-ffind-test--touch (expand-file-name ".myanchor" root))
      ;; Passing the custom identifier as the second argument finds the root.
      (should (equal (pel-ffind-project-directory-of file '(".myanchor"))
                     (directory-file-name (expand-file-name root)))))))

;;; --------------------------------------------------------------------------
;;; Section 7 – pel-ffind-project-name
;;; --------------------------------------------------------------------------

(defun pel-ffind-test--make-project (root-path &optional settings)
  "Build a minimal `pel-dev-projects' entry with ROOT-PATH and SETTINGS.
If SETTINGS is omitted an empty settings list is used."
  (list "test-project" (list root-path (or settings '()))))

(ert-deftest pel-ffind-project-name/returns-name-for-matching-project ()
  "Returns the project name when the discovered root matches pel-dev-projects."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file   (expand-file-name "src/hello.c" root))
           (root-path  (directory-file-name (expand-file-name root)))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (list (pel-ffind-test--make-project root-path))))
      (pel-ffind-test--touch src-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-name src-file) "test-project")))))

(ert-deftest pel-ffind-project-name/returns-nil-when-no-anchor ()
  "Returns nil when no anchor file is found so no project root is resolved."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file (expand-file-name "src/hello.c" root))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects                       '()))
      (pel-ffind-test--touch src-file)
      (should (null (pel-ffind-project-name src-file))))))

(ert-deftest pel-ffind-project-name/returns-nil-when-no-dev-projects-match ()
  "Returns nil when an anchor is found but no pel-dev-projects entry matches."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file  (expand-file-name "src/hello.c" root))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects
            (list (pel-ffind-test--make-project "/completely/different/path"))))
      (pel-ffind-test--touch src-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (null (pel-ffind-project-name src-file))))))

(ert-deftest pel-ffind-project-name/selects-first-matching-project ()
  "When multiple projects match, the first matching one is used."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file  (expand-file-name "src/hello.c" root))
           (root-path (directory-file-name (expand-file-name root)))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects
            (list (list "first-project"  (list root-path '()))
                  (list "second-project" (list root-path '())))))
      (pel-ffind-test--touch src-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-name src-file) "first-project")))))

;;; --------------------------------------------------------------------------
;;; Section 8 – pel-ffind-project-settings
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-project-settings/returns-settings-for-matching-project ()
  "Returns the complete settings list when the project root matches."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file   (expand-file-name "src/hello.c" root))
           (root-path  (directory-file-name (expand-file-name root)))
           (setting    '((c) ("/some/tree") ("gcc") () () ()))
           (settings   (list setting))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects
            (list (list "p" (list root-path settings)))))
      (pel-ffind-test--touch src-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-settings src-file) settings)))))

(ert-deftest pel-ffind-project-settings/returns-nil-when-no-project ()
  "Returns nil when no project root is found."
  (pel-ffind-test--with-temp-dir root
    (let* ((src-file (expand-file-name "src/hello.c" root))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects                       '()))
      (pel-ffind-test--touch src-file)
      (should (null (pel-ffind-project-settings src-file))))))

;;; --------------------------------------------------------------------------
;;; Section 9 – pel-ffind-project-lang-setting / tools / envvars /
;;;             exclude-regexps
;;; --------------------------------------------------------------------------
;;
;; Note: pel-language-of deduces the language from the file extension.
;; We use .c files (→ 'c) throughout this section.

(defun pel-ffind-test--setup-lang-project (root c-setting)
  "Create a test project in ROOT dir with C-SETTING as the single C setting.
Return the `pel-dev-projects' list."
  (list (list "lang-test-project"
              (list (directory-file-name (expand-file-name root))
                    (list c-setting)))))

(ert-deftest pel-ffind-project-lang-setting/returns-matching-language-setting ()
  "Returns the setting whose language list includes the file language."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file    (expand-file-name "src/main.c" root))
           (c-setting '((c)   ("/c-tree")   ("gcc")  () () ()))
           (py-setting '((python) ("/py-tree") () () () ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects
            (list (list "p" (list (directory-file-name (expand-file-name root))
                                  (list c-setting py-setting))))))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-lang-setting c-file) c-setting)))))

(ert-deftest pel-ffind-project-lang-setting/nil-language-matches-all ()
  "A nil languages slot in a setting matches any language."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file    (expand-file-name "src/main.c" root))
           (universal '(nil ("/shared") ("common-tool") () () ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (pel-ffind-test--setup-lang-project root universal)))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-lang-setting c-file) universal)))))

(ert-deftest pel-ffind-project-lang-setting/returns-nil-when-no-lang-match ()
  "Returns nil when no setting matches the file's language."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file    (expand-file-name "src/main.c" root))
           ;; Only a Python setting — no match for C
           (py-setting '((python) () () () () ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects
            (list (list "p" (list (directory-file-name (expand-file-name root))
                                  (list py-setting))))))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (null (pel-ffind-project-lang-setting c-file))))))

(ert-deftest pel-ffind-project-lang-tools/returns-tool-list ()
  "Returns the tools list from the language-specific project setting."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file  (expand-file-name "src/prog.c" root))
           (setting '((c) () ("gcc" "clang") () () ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (pel-ffind-test--setup-lang-project root setting)))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-lang-tools c-file) '("gcc" "clang"))))))

(ert-deftest pel-ffind-project-lang-tools/returns-nil-when-no-tools ()
  "Returns nil when the tool list is empty."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file  (expand-file-name "src/prog.c" root))
           (setting '((c) () () () () ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (pel-ffind-test--setup-lang-project root setting)))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (null (pel-ffind-project-lang-tools c-file))))))

(ert-deftest pel-ffind-project-lang-envvars/returns-envvar-list ()
  "Returns the environment-variable list from the language-specific setting."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file  (expand-file-name "src/prog.c" root))
           (setting '((c) () () () ("CPATH" "C_INCLUDE_PATH") ()))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (pel-ffind-test--setup-lang-project root setting)))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-lang-envvars c-file)
                     '("CPATH" "C_INCLUDE_PATH"))))))

(ert-deftest pel-ffind-project-lang-exclude-regexps/returns-regexp-list ()
  "Returns the exclusion regexp list from the language-specific setting."
  (pel-ffind-test--with-temp-dir root
    (let* ((c-file  (expand-file-name "src/prog.c" root))
           (setting '((c) () () () () ("\\.o$" "\\.a$")))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects (pel-ffind-test--setup-lang-project root setting)))
      (pel-ffind-test--touch c-file)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (should (equal (pel-ffind-project-lang-exclude-regexps c-file)
                     '("\\.o$" "\\.a$"))))))

;;; --------------------------------------------------------------------------
;;; Section 10 – pel-ffind integration tests (require find/fd)
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind/finds-file-in-explicit-tree ()
  "pel-ffind finds an existing file when given an explicit directory tree path."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let ((target (expand-file-name "subdir/needle.h" root)))
      (pel-ffind-test--touch target)
      (pel-ffind-test--with-global-tool-reset
        (let ((pel-ffind-executable 'find)
              (result (pel-ffind "needle.h" root)))
          (should (consp result))
          (should (member target result)))))))

(ert-deftest pel-ffind/returns-nil-for-absent-file ()
  "pel-ffind returns nil (empty list) when the target file does not exist."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (pel-ffind-test--with-global-tool-reset
      (let ((pel-ffind-executable 'find))
        (should (null (pel-ffind "does-not-exist-xyzzy.h" root)))))))

(ert-deftest pel-ffind/exclusive-tree-dpaths-limits-search-scope ()
  "When TREE-DPATHS is non-nil and INCLUSIVE is nil, only TREE-DPATHS is searched."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root1
    (pel-ffind-test--with-temp-dir root2
      (let ((file-in-root1 (expand-file-name "secret.h" root1)))
        (pel-ffind-test--touch file-in-root1)
        (pel-ffind-test--with-global-tool-reset
          (let ((pel-ffind-executable 'find))
            ;; Search exclusively in root2 – the file lives in root1.
            (should (null (pel-ffind "secret.h" root2 nil)))))))))

(ert-deftest pel-ffind/inclusive-search-includes-default-directory ()
  "When INCLUSIVE is non-nil, both TREE-DPATHS and default-directory are searched."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir extra-tree
    (pel-ffind-test--with-temp-dir proj-root
      (let* ((file-in-proj  (expand-file-name "proj-file.h" proj-root))
             (file-in-extra (expand-file-name "extra-file.h" extra-tree))
             (default-directory proj-root))
        (pel-ffind-test--touch file-in-proj)
        (pel-ffind-test--touch file-in-extra)
        (pel-ffind-test--with-global-tool-reset
          (with-temp-buffer
            (setq pel--ffind-search-initialized t
                  pel--ffind-searched-dirs      nil
                  pel--ffind-searched-dir-trees nil
                  pel--ffind-exclusion-regexps  nil)
            (let ((pel-ffind-executable 'find)
                  (default-directory proj-root))
              ;; INCLUSIVE search: extra-tree listed in TREE-DPATHS,
              ;; proj-root comes from default-directory.
              (let ((result (pel-ffind "*.h" extra-tree 'inclusive)))
                (should (member file-in-extra result))
                (should (member file-in-proj result))))))))))

(ert-deftest pel-ffind/exclusion-regexps-filter-results ()
  "Files matching an exclusion regexp are removed from the results."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let* ((keep-file    (expand-file-name "src/keep.h"      root))
           (exclude-file (expand-file-name "build/exclude.h" root))
           (default-directory root))
      (pel-ffind-test--touch keep-file)
      (pel-ffind-test--touch exclude-file)
      (pel-ffind-test--with-global-tool-reset
        (with-temp-buffer
          ;; Pre-populate the cache with an exclusion regexp for /build/.
          (setq pel--ffind-search-initialized t
                pel--ffind-searched-dirs      nil
                pel--ffind-searched-dir-trees nil
                pel--ffind-exclusion-regexps  '("/build/"))
          (let ((pel-ffind-executable 'find)
                (default-directory root))
            ;; nil TREE-DPATHS → uses the project/default-dir branch which
            ;; respects pel--ffind-exclusion-regexps.
            (let ((result (pel-ffind "*.h")))
              (should-not (member exclude-file result))
              (should     (member keep-file    result)))))))))

(ert-deftest pel-ffind/no-ignore-bypasses-exclusion-regexps ()
  "When NO-IGNORE is non-nil, exclusion regexps are not applied."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let* ((would-be-excluded (expand-file-name "build/target.h" root))
           (default-directory root))
      (pel-ffind-test--touch would-be-excluded)
      (pel-ffind-test--with-global-tool-reset
        (with-temp-buffer
          (setq pel--ffind-search-initialized t
                pel--ffind-searched-dirs      nil
                pel--ffind-searched-dir-trees nil
                pel--ffind-exclusion-regexps  '("/build/"))
          (let ((pel-ffind-executable 'find)
                (default-directory root))
            ;; NO-IGNORE = t → result should include the normally-excluded file.
            (let ((result (pel-ffind "*.h" nil nil 'no-ignore)))
              (should (member would-be-excluded result)))))))))

(ert-deftest pel-ffind/directory-prefix-in-fname-filters-results ()
  "When FNAME has a directory portion, only files inside that directory match."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let* ((match    (expand-file-name "subdir/wanted.h"      root))
           (no-match (expand-file-name "otherdir/wanted.h"    root))
           (default-directory root))
      (pel-ffind-test--touch match)
      (pel-ffind-test--touch no-match)
      (pel-ffind-test--with-global-tool-reset
        (with-temp-buffer
          (setq pel--ffind-search-initialized t
                pel--ffind-searched-dirs      nil
                pel--ffind-searched-dir-trees nil
                pel--ffind-exclusion-regexps  nil)
          (let ((pel-ffind-executable 'find)
                (default-directory root))
            ;; FNAME includes a partial directory: only files under subdir/ match.
            (let ((result (pel-ffind "subdir/wanted.h")))
              (should     (member match    result))
              (should-not (member no-match result)))))))))

;;; --------------------------------------------------------------------------
;;; Section 11 – pel-generic-find-file integration tests
;;; --------------------------------------------------------------------------

(ert-deftest pel-generic-find-file/finds-file-in-project-root-tree ()
  "Searches the whole project root tree when an anchor file is present."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let* ((target           (expand-file-name "deep/sub/myheader.h" root))
           (default-directory (expand-file-name "deep/sub" root))
           (pel-project-root-identifiers           '(".git"))
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects '()))
      (pel-ffind-test--touch target)
      (pel-ffind-test--touch (expand-file-name ".git" root))
      (pel-ffind-test--with-global-tool-reset
        (let ((pel-ffind-executable 'find)
              (result (pel-generic-find-file "myheader.h")))
          (should (consp result))
          (should (member target result)))))))

(ert-deftest pel-generic-find-file/falls-back-to-default-directory-tree ()
  "When no anchor is found, searches the default-directory tree."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let* ((target           (expand-file-name "fallback.h" root))
           (default-directory root)
           ;; No anchor identifiers → no project root discovered.
           (pel-project-root-identifiers           '())
           (pel-project-restricted-root-identifiers '())
           (pel-dev-projects '()))
      (pel-ffind-test--touch target)
      (pel-ffind-test--with-global-tool-reset
        (let ((pel-ffind-executable 'find)
              (result (pel-generic-find-file "fallback.h")))
          (should (consp result))
          (should (member target result)))))))

(ert-deftest pel-generic-find-file/returns-nil-for-nonexistent-file ()
  "Returns nil when the searched file does not exist in any searched directory."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir root
    (let ((default-directory root)
          (pel-project-root-identifiers           '())
          (pel-project-restricted-root-identifiers '())
          (pel-dev-projects '()))
      (pel-ffind-test--with-global-tool-reset
        (let ((pel-ffind-executable 'find))
          (should (null (pel-generic-find-file "no-such-file-xyzzy.h"))))))))

(ert-deftest pel-generic-find-file/extra-tree-dpaths-extend-search ()
  "Extra TREE-DPATHS directories are searched in addition to the project root."
  (skip-unless (executable-find "find"))
  (pel-ffind-test--with-temp-dir proj-root
    (pel-ffind-test--with-temp-dir extra-dir
      (let* ((file-in-extra (expand-file-name "extra.h" extra-dir))
             (default-directory proj-root)
             (pel-project-root-identifiers           '(".git"))
             (pel-project-restricted-root-identifiers '())
             (pel-dev-projects '()))
        (pel-ffind-test--touch file-in-extra)
        (pel-ffind-test--touch (expand-file-name ".git" proj-root))
        (pel-ffind-test--with-global-tool-reset
          (let ((pel-ffind-executable 'find)
                (result (pel-generic-find-file "extra.h" extra-dir)))
            (should (consp result))
            (should (member file-in-extra result))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind-test)

;;; pel-ffind-test.el ends here

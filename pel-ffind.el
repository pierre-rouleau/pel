;;; pel-ffind.el --- PEL file find utilities  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-19 12:43:16 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2024-2026  Pierre Rouleau
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
;; This file provides two file finding functions:
;;
;; - `pel-ffind' is the most capable; it find files located in the list of
;;   directories and directory trees corresponding to the language-specific
;;   project setting defined identified by the `pel-dev-projects',
;;   `pel-dev-tools' and `pel-dev-libraries' user-options.  This includes
;;   directories that can be located in separate tree locations in the file
;;   system, not only inside the current project directory tree like the
;;   directories used by the tool and libraries used by the project.
;;
;; - `pel-generic-find-file' search for files inside the directory tree of the
;;   current project, nothing else.
;;
;;
;; The code hierarchical organization is show below, where '*' identifies a
;; command, '-' a function and '.' a reference to a function implemented
;; above.
;;
;;
;; Tool Chain Overriding:
;;  * `pel-ffind-set-devtool-name'
;;    - `pel--dev-tool-names'
;;
;; Search Tool Identification
;;  - `pel--ffind-command'
;;    - `pel--ffind-select-tool'
;;    - `pel--ffind-dirname-expanded'
;;
;; Buffer Settings Cache and Clear Command
;;  * `pel-ffind-reset-cache'
;;
;;
;; Extraction of Project Information
;;    The '<<' mark functions that extract the fields of `pel-dev-projects':
;;
;;  - `pel-ffind-project-name'                 <<
;;  - `pel-ffind-project-rootdir'              <<
;;    - `pel-ffind-project-directory-of'
;;  - `pel--ffind-project-lang-directories'    <<
;;    - `pel-ffind-project-lang-setting'
;;      . `pel-ffind-project-name'
;;      - `pel-ffind-project-settings'
;;    . `pel-ffind-env-tool-names'
;;    - `pel--push-expanded-dir-to-dirs'
;;      -`pel--warn-invalid-dir'
;;  - `pel-ffind-project-lang-tools'           <<
;;  - `pel-ffind-project-lang-envvars'         <<
;;  - `pel-ffind-project-lang-exclude-regexps' <<

;; Extraction from Environment
;;  - `pel-ffind-env-tool-names'

;; Identify directories and directory-trees to search
;;      - `pel--ffind-set-project-dirs'
;;        . `pel--ffind-project-lang-directories'
;;
;; Finding file in language-specific project defined in `pel-dev-projects'
;;    - `pel-ffind'
;;      . `pel--ffind-command'
;;      . `pel--ffind-set-project-dirs'

;; Finding file in simple project
;;  - `pel-generic-find-file'
;;    . `pel-ffind-project-rootdir'
;;    . `pel-ffind'
;;
;;
;; Help on customization and search settings
;;  * `pel-ffind-show-status'

;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                  ; use: `pel-system-is-macos-p',
;;                                    ;      `pel-major-mode-symbol-value-or'
;;                                    ;      `pel-major-mode-of-file'
;;                                    ;      `pel-language-of'
;;                                    ;      `pel-string-for'
;;                                    ;      `pel-path='
(require 'pel--options)               ; use: `pel-ffind-executable'
(eval-when-compile (require 'subr-x)) ; use: `string-join', `string-trim'
;; subr is always loaded              ; use  `y-or-n-p'
(require 'seq)                        ; use: `seq-filter', `seq-uniq'
(require 'cl-lib)                     ; use: `cl-set-difference'
(require 'cus-edit)                   ; use: `customize-option'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Tool Chain Overriding
;;  =====================

(defvar-local pel--ffind-overriding-toolchain-name nil
  "The overriding name of the tool scoping file search in the buffer.

When non-nil this is the name of the tool chain selected dynamically for
the current buffer, overriding any tool name selected by the value of
PEL_DEV_TOOLS_FOR_'LANG' environment variable (if it exists) and the the
tool names specified inside the `pel-dev-libraries' data structure for
the current project for the language.
It is selected by the `pel-ffind-set-devtool-name ' command.")

(defun pel--dev-tool-names ()
  "Return a list of possible tool names available in `pel-dev-tools'."
  (sort (mapcar #'car pel-dev-tools) #'string<))

;;-pel-autoload
(defun pel-ffind-set-devtool-name (tool-name)
  "Prompt/select a new development TOOL-NAME key for the buffer.

When called in code, the TOOL-NAME must be one of the tool name key
identified in the `pel-dev-tools' user-option or nil.  Interactively, the
command prompt the user for one of the available tool names."
  (interactive
   (let ((available-tool-names (pel--dev-tool-names)))
     (list
      (when available-tool-names
        (completing-read
         (format "Select file finder tool name%s: "
                 (pel-string-for pel--ffind-overriding-toolchain-name " (" ") "))
         available-tool-names)))))
  (if tool-name
      (setq-local pel--ffind-overriding-toolchain-name tool-name)
    (when (y-or-n-p "No tool identified in pel-dev-tools; customize it?")
      (customize-option 'pel-dev-tools))))

;; ---------------------------------------------------------------------------
;;* Search Tool Identification
;;  ==========================

(defvar pel--ffind-executable nil
  "Adjusted value of `pel-ffind-executable'.

Identifies the search tool actually used.  When `pel-ffind-executable'
is set to auto, it replaces auto by fdfind, fd or find.  Its value is
computed by `pel--ffind-select-tool' which is called by
`pel--ffind-command'.  That function stores the new value.

When `pel-ffind-executable' is a command string, the value
\\='command-string is stored here and the code uses the command string
from the `pel-ffind-executable' user-option.")

(defvar pel--ffind-path nil
  "Full path of fd/fdfind/find executable used.")

;; --

(defun pel--ffind-select-tool ()
  "Select the search tool based on `pel-ffind-executable' choice.
Return a (symbol . string) cons cell with the two values to store in
`pel--ffind-executable' and `pel--ffind-path' when `pel-ffind-executable'
value is fd or find.  Return ('command-string . command) when a command line
is specified."

  (let ((choice nil)
        (exe-path nil))
    (cond
     ;; -- When tool is selected automatically: Prioritize: fdfind, fd, find
     ;; -- When fd is selected, check for fdfind and fd. If none is available
     ;;    select find but issue a warning.  Don't issue the warning when the
     ;;    choice is auto.
     ((memq pel-ffind-executable '(auto fd))
      ;; Some system (eg. Debian use fdfind instead of fd, allowing another,
      ;; unrelated fd command: search for fdfind first.
      (setq exe-path (executable-find "fdfind"))
      (unless exe-path
        (setq exe-path (executable-find "fd")))
      (if exe-path
          (setq choice 'fd)
        (unless (eq pel-ffind-executable 'auto)
          (display-warning 'pel-ffind
                           "\
pel-ffind-executable requests fd but neither fd nor fdfind is available.
Using find instead."
                           :warning))
        (setq choice 'find)
        (setq exe-path (executable-find "find"))))
     ;;
     ;; -- When find is selected use it.
     ((eq pel-ffind-executable 'find)
      (setq choice 'find)
      (setq exe-path (executable-find "find")))
     ;;
     ;; -- When a command string is specified, use it
     ((stringp pel-ffind-executable)
      (setq choice 'command-string)
      (setq exe-path pel-ffind-executable)))
     ;; -- if nothing is valid signal an error
    (unless exe-path
      (user-error
       "No command specified and none of fdfind, fd, or find tool available on this system!"))
    (cons choice exe-path)))

(defun pel--ffind-dirname-expanded (dirname)
  "Return DIRNAME inside single quotes, expanded and without trailing slash.
Substitute the ~ with the home directory.
Replace the name of environment variables with their values."
  (format "'%s'" (shell-quote-argument  ; escape any single quote that may be in path
                  (directory-file-name  ; remove any trailing slash
                   ;; fully expand dirname, expand all environment variables
                   ;; and expand ~ if it is used.
                   (pel-expanded-path dirname)))))

(defun pel--ffind-command (filename tree-dpaths)
  "Return a ffind command searching for FILENAME in directory trees.

FILENAME may be a glob file pattern and may start with an absolute or
relative directory path.

TREE-DPATHS may specify a single directory path string or a list of directory
path strings.  Each directory path may start with \"~\" and $VARNAME or
${VARNAME} style environment variable.  These are all expanded.

The returned command will produce a list of files sorted in lexicographic
order.

The VCS-ignore capability of fd is not used, so all files are found
whether the VCS setting (like the .gitignore file) is set to ignore them."
  (setq tree-dpaths (pel-list-of tree-dpaths))
  (let ((file-basename (pel-shell-quote-path-keep-glob
                        (file-name-nondirectory filename)))
        (dirnames (if (> (length tree-dpaths) 1)
                      (string-join (mapcar #'pel--ffind-dirname-expanded
                                           tree-dpaths)
                                   " ")
                    (pel--ffind-dirname-expanded (car tree-dpaths)))))

    ;; Initialize tool selection and its path if not already done.
    (unless pel--ffind-executable
      (let ((choice.exe-path (pel--ffind-select-tool)))
        (setq pel--ffind-executable (car choice.exe-path))
        (setq pel--ffind-path (cdr choice.exe-path))))

    ;; Return the formatted command string
    (cond
     ;; -- using fd (or fdfind):
     ((eq pel--ffind-executable 'fd)
      ;; fd sorts by default.  The -g option (for globs) can't handle file
      ;; names that start with a dash: it must be preceded with a backslash.
      (when (pel-string-starts-with-p file-basename "-")
        (setq file-basename (concat "\\" file-basename)))
      (format "%s --type f --color never --no-ignore-vcs -g '%s' %s"
              pel--ffind-path
              file-basename
              dirnames))
     ;;
     ;; -- using find:
     ;; find handles file names that start with a dash properly: no need for
     ;; special escape.
     ((eq pel--ffind-executable 'find)
      ;; On macOS find requires the -s option to sort.
      ;; That option is not supported on Linux
      (let ((sort-option (if pel-system-is-macos-p "-s" "")))
        (format "%s %s %s -name '%s' -type f"
                pel--ffind-path
                sort-option
                dirnames
                file-basename)))
     ;;
     ;; -- using an explicit command line:
     ;; A string with the following keywords replaced:
     ;; {FNAME}    : the base name of the file
     ;; {DIRNAMES} : a space separated list of directory names to search
     ((eq pel--ffind-executable 'command-string)
      (let ((cmd pel-ffind-executable))
        (setq cmd (replace-regexp-in-string "{FNAME}"
                                            file-basename
                                            cmd
                                            'fixed-case
                                            'literal))
        (setq cmd (replace-regexp-in-string "{DIRNAMES}"
                                            dirnames
                                            cmd
                                            'fixed-case
                                            'literal))
        cmd)))))

;; ---------------------------------------------------------------------------
;;* Buffer Settings Cache and Clear Command
;;  =======================================

(defvar-local pel--ffind-search-initialized nil
  "Set to t when search directories and trees are initialized.")

(defvar-local pel--ffind-searched-dirs nil
  "List of directories searched by `pel-ffind' in the current buffer.
Each directory listed here is searched non-recursively.
The list is set by `pel--ffind-set-project-dirs'.")

(defvar-local pel--ffind-searched-dir-trees nil
  "List of directory trees searched by `pel-ffind' in the current buffer.
Each directory listed here is searched recursively.
The list is set by `pel--ffind-set-project-dirs'.")

(defvar-local pel--ffind-exclusion-regexps nil
  "List of regular expression strings identifying files to exclude.
The list is set by `pel--ffind-set-project-dirs'.")
;; --

(defun pel-ffind-reset-cache (&optional clear-search-tool silently)
  "Clear `pel-dev-projects' searched directory cache for current buffer.

When CLEAR-SEARCH-TOOL is non-nil also clear the file search tool
settings adjusted from the `pel-ffind-executable' user-option.
Print a message showing it's been done unless SILENTLY is non-nil.

Clear the cache after changing the `pel-dev-projects', `pel-dev-tools'
and `pel-dev-libraries' user-options to activate these changes.  Also
clear the search-tool cache after changing `pel-ffind-executable'
user-option and/or installing a new file search tool in your system."
  (interactive "P")
  (setq pel--ffind-searched-dirs nil
        pel--ffind-searched-dir-trees nil
        pel--ffind-exclusion-regexps  nil
        pel--ffind-search-initialized nil)
  (when clear-search-tool
    (setq pel--ffind-executable nil
          pel--ffind-path       nil))
  (unless silently
    (message "Cleared ffind directory cache for %s%s."
             (pel-current-buffer-filename)
             (if clear-search-tool
                 ", and search tool."
               ""))))

;; ---------------------------------------------------------------------------
;;* Extraction of Project Information
;;  =================================


(defun pel-ffind-project-name (&optional filename)
  "Return the project name of FILENAME or currently visited file.
Return nil if no project name is found.
The project name is extracted from information found in `pel-dev-projects'
considering the name of the currently visited file or the specified FILENAME."
  (let ((fname (or filename (buffer-file-name))))
    (when fname
      (let ((project-root (pel-ffind-project-directory-of fname))
            (project-name nil))
        ;; Find the project root that matches the current project root: then
        ;; return its associated project name.
        (catch 'pel--ffind-pn-break
          (dolist (proj pel-dev-projects)
            (when (pel-path= (pel-dev-project.root-dir proj) project-root)
              (setq project-name (pel-dev-project.name proj))
              (throw 'pel--ffind-pn-break nil))))
        project-name))))

;; ----
(defun pel-ffind-project-directory-of (pathname
                                       &optional project-root-identifiers)
  "Find and return the project root directory of PATHNAME.

PATHNAME must be an absolute path that may start with ~ and embed any
$VARNAME and ${VARNAME} style environment variable(s).

Identify the project root directory of PATHNAME using the anchor files
specified in the `pel-project-root-identifiers' user-option and the ones
in PROJECT-ROOT-IDENTIFIERS list if specified.

Unless it finds a restrictive anchor identified by
`pel-project-restricted-root-identifiers', the upward search for the root
directory continues and if multiple anchors are found at different tree
levels, the outermost (shortest path) directory is selected.  This
broadens the file search scope when nested projects exist while still
allowing a restricted project scope for some projects.

Return a directory name expanded and without trailing slash if found,
nil otherwise."
  (let ((cur-dir (file-name-directory (pel-expanded-path pathname)))
        (identifiers pel-project-root-identifiers)
        (directory nil)
        (found-dir nil))
    ;; make a list of all project root identifiers
    (dolist (fname project-root-identifiers)
      (unless (member fname identifiers)
        (push fname identifiers)))
    ;; also search for restricted root identifiers so they can be found and
    ;; trigger the stop.
    (dolist (fname pel-project-restricted-root-identifiers)
      (unless (member fname identifiers)
        (push fname identifiers)))
    ;; search project root from current directory up looking for a
    ;; project root identifier file.  Stops at any restricted anchor if it
    ;; finds one. Otherwise retain the shortest directory path found
    ;; to allow nested projects and keep the more encompassing one, broadening
    ;; the file search: anyway if more than 1 file found the user will be
    ;; prompted.
    ;;
    (catch 'pel-ffind--break
      (dolist (anchor-fname identifiers)
        (setq found-dir (locate-dominating-file cur-dir anchor-fname))
        (when found-dir
          ;; stop on any restricted root anchor
          (when (member anchor-fname pel-project-restricted-root-identifiers)
            (setq directory found-dir)
            (throw 'pel-ffind--break nil))
          (if directory
              (when (< (length found-dir) (length directory))
                (setq directory found-dir))
            (setq directory found-dir)))))
    ;; Return a directory name expanded and without trailing slash.
    (when directory
      (expand-file-name (directory-file-name directory)))))

(defun pel-ffind-project-rootdir (&optional project-root-identifiers)
  "Find and return the project root directory of file in current buffer.

Search project root directory using the anchor files specified in the
`pel-project-root-identifiers' user-option and the ones in
PROJECT-ROOT-IDENTIFIERS list if specified.

Unless it finds a restrictive anchor identified by
`pel-project-restricted-root-identifiers', the upward search for the root
directory continues and if multiple anchors are found at different tree
levels, the outermost (shortest path) directory is selected.  This
broadens the file search scope when nested projects exist while still
allowing a restricted project scope for some projects.

Return a directory name expanded and without trailing slash if found,
nil otherwise."
  (pel-ffind-project-directory-of default-directory
                                  project-root-identifiers))

;; ----
(defun pel-ffind-project-settings (&optional filename)
  "Return the project settings list for current project or one using FILENAME.
Return nil if no project is found.
The inspected project is identified by the specified FILENAME if any,
otherwise by the currently visited file.
The project name is extracted from information found in `pel-dev-projects'
considering the name of the currently visited file or the specified FILENAME."
  (let ((fname (or filename (buffer-file-name))))
    (when fname
      (let ((project-root (pel-ffind-project-directory-of fname))
            (project-settings nil))
        (catch 'pel--ffind-pd-break
          (dolist (proj pel-dev-projects)
            (when (pel-path= (pel-dev-project.root-dir proj)  project-root)
              (setq project-settings (pel-dev-project.settings proj))
              (throw 'pel--ffind-pd-break nil))))
        project-settings))))

(defun pel-ffind-project-lang-setting (&optional filename)
  "Return language specific project setting of current or FILENAME project.

Return nil if no project is found or if no setting appropriate for the
specific language is found.

The inspected project and the language is identified by the specified
FILENAME if any, otherwise it is identified by the currently visited
file.

NOTE: For any project, there should only be one setting corresponding to
any language inside the pel-dev-projects' data structure.  If a project
settings identifies more than one setting for a given programming
language only the first one will ever be used and all the others will be
ignored but will instead cause the generation of a warning.  If you see
these warning, you should fix `pel-dev-projects' contents."
  (let ((project-settings (pel-ffind-project-settings filename))
        (lang (pel-language-of filename))
        (lang-specific-setting nil)
        )
    (dolist (proj-setting project-settings)
      (let ((project-languages
             (pel-dev-project.setting.languages proj-setting)))
        ;; Each project can be language agnostic or specific to one or many.
        (when (or (null project-languages)
                  (memq lang project-languages))
          ;; However only one of the project settings can be used.
          ;; If more than one would apply issue a warning.
          (if lang-specific-setting
              (display-warning
               'pel-ffind-project-lang-setting
               (format
                "Multiple setting for %s in pel-dev-project %s."
                lang (pel-ffind-project-name filename))
               :warning)
            (setq lang-specific-setting proj-setting)))))
    lang-specific-setting))

;; --

(defun pel--warn-invalid-dir (dir expanded-dir)
  "Display a warning about invalid DIR with its EXPANDED_DIR expansion."
  (display-warning
   'pel-dev-projects
   (format "pel-dev-projects identifies an invalid directory: PLEASE FIX!
 - Invalid entry: %s%s"
           dir
           (if (string= dir expanded-dir)
               ""
             (format "
 - It expands to: %s" expanded-dir)))
   :warning))

(defmacro pel--push-expanded-dir-to-dirs (dir dirs error-detected)
  "Push existing expanded DIR to DIRS list.
Expand ~ and environment variables in DIR and check its presence.
If it is present push it to the DIRS list otherwise display a warning
and set the ERROR-DETECTED symbol to t."
  ;; Note: the macro evaluates each argument once to prevent
  ;;       problems induced by multiple side effects executions.
  `(let* ((dt ,dir)
          (expanded-dir (pel-expanded-path dt)))
     (if (file-directory-p expanded-dir)
         (push expanded-dir ,dirs)
       (pel--warn-invalid-dir dt expanded-dir)
       (setq ,error-detected t))))

(defun pel--ffind-project-lang-directories (&optional filename)
  "Return language specific directory list of current or FILENAME project.

The inspected project and language is identified by the specified
FILENAME if any, otherwise it is identified by the currently visited
file.

The function returns a cons of 2 lists: (directories directory-trees).
It includes all directories and directory trees specified directly and
indirectly by the project tools and libraries identified in the project
settings stored in the `pel-dev-projects' user-option.

Note that if the environment holds a PEL_DEV_TOOLS_FOR_\\='LANG\\='
variable, the content of that variable overrides the tool names for all
the files that use the corresponding language LANG.

All paths in both lists are absolute and are fully expanded.
The presence of every directory is checked.
If a directory does not exists it is removed from the list and the function
issues a warning describing the error."
  (let ((proj-lang-setting (pel-ffind-project-lang-setting filename))
        (directories ())
        (directory-trees ())
        (exclusion-regexps nil)
        (detected-error nil)
        dirs/dir-trees
        dirs
        dir-trees)
    (when proj-lang-setting
      ;; Add each expanded directory tree to the list.
      (dolist (dir-tree (pel-dev-project.setting.dir-trees proj-lang-setting))
        (pel--push-expanded-dir-to-dirs dir-tree directory-trees
                                        detected-error))

      ;; Get directories associated with tool names.
      ;; Tool names are taken from the environment variable named
      ;; PEL_DEV_TOOLS_FOR_'LANG' if it exists, otherwise use the tool names
      ;; specified inside the `pel-dev-projects' data structure for the
      ;; current project and language.
      (dolist (tool-name
               (or (when pel--ffind-overriding-toolchain-name
                     (pel-list-of pel--ffind-overriding-toolchain-name))
                   (pel-ffind-env-tool-names (pel-language-of filename))
                   (pel-dev-project.setting.tools proj-lang-setting)))
        ;; Add directories and directory trees associated with each tool-name.
        ;; Expand each path before adding it.
        (setq dirs/dir-trees (pel-dev-tool-dirs tool-name))
        (setq dirs      (car dirs/dir-trees))
        (setq dir-trees (cadr dirs/dir-trees))
        (when dirs
          (dolist (dir dirs)
            (pel--push-expanded-dir-to-dirs dir directories
                                            detected-error)))
        (when dir-trees
          (dolist (dir-tree dir-trees)
            (pel--push-expanded-dir-to-dirs dir-tree directory-trees
                                            detected-error))))

      ;; Get directories associated with the library(ies) associated with
      ;; the project/language from the `pel-dev-projects' data structure.
      (dolist (lib-name (pel-dev-project.setting.libs proj-lang-setting))
        ;; Add directories and directory trees associated with each library
        ;; name.  Expand each path before adding it.
        (setq dirs/dir-trees (pel-dev-lib-dirs lib-name))
        (setq dirs      (car dirs/dir-trees))
        (setq dir-trees (cadr dirs/dir-trees))
        (when dirs
          (dolist (dir dirs)
            (pel--push-expanded-dir-to-dirs dir directories
                                            detected-error)))
        (when dir-trees
          (dolist (dir-tree dir-trees)
            (pel--push-expanded-dir-to-dirs dir-tree directory-trees
                                            detected-error))))

      ;; Get directories associated with environment variables identified for
      ;; the project for the language.
      (dolist (varname (pel-dev-project.setting.envvars
                        proj-lang-setting))
        ;; Add the directories identified by the path environment variables
        ;; Note that the variable names may hold $VARNAME to allow controlled
        ;; name composition.  Expand each path before adding it.
        (dolist (dirpath (pel-envvar-value-list
                          (pel-substitute-env-vars varname)))
          (pel--push-expanded-dir-to-dirs dirpath directories
                                          detected-error)))

      ;; Get the list of regular expressions used to detect path names to
      ;; exclude from the list of files for the project for the language.
      (setq exclusion-regexps (pel-dev-project.setting.exclude-regexps
                               proj-lang-setting)))

    ;; If any error was detected in the directories and directory trees
    ;; identified in `pel-dev-projects', the above code generated warnings
    ;; requesting user to modify the specs.  Clear the cached values of the
    ;; directory settings to allow using the changes done by the user.
    (when detected-error
      (message "Please fix pel-dev-projects errors.\
 Then execute pel-ffind-reset-cache to activate your changes."))
    ;; Remove any duplicates from lists kept in the original order.
    (setq directories     (reverse (seq-uniq directories #'string=))
          directory-trees (reverse (seq-uniq directory-trees #'string=)))
    ;; If a directory is identified in both lists remove if from the
    ;; directories list and keep it in the directory trees list.
    (setq directories (cl-set-difference directories directory-trees
                                         :test #'string=))
    (list directories directory-trees exclusion-regexps)))

(defun pel-ffind-project-lang-tools (&optional filename)
  "Return the list of tool names specific to the FILENAME or current buffer.

Return the list of tool names identified by `dev-pel-projects' for the project
and language specific for the FILENAME or currently visited file.  Return nil
if there are none or if the buffer is not visiting a file."
  (pel-dev-project.setting.tools (pel-ffind-project-lang-setting filename)))

(defun pel-ffind-project-lang-envvars (&optional filename)
  "Return the list of envvars specific to the FILENAME or current buffer.

Return the list of environment variables identified by `dev-pel-projects'
for the project and language specific for the FILENAME or currently
visited file.  Return nil if there are none or if the buffer is not
visiting a file."
  (pel-dev-project.setting.envvars (pel-ffind-project-lang-setting filename)))

(defun pel-ffind-project-lang-exclude-regexps (&optional filename)
  "Return the list of exclude regexps specific to the FILENAME or current buffer.

Return the list of exclude regexps identified by `dev-pel-projects'
for the project and language specific for the FILENAME or currently
visited file.  Return nil if there are none or if the buffer is not
visiting a file."
  (pel-dev-project.setting.exclude-regexps (pel-ffind-project-lang-setting filename)))

;; ---------------------------------------------------------------------------
;;* Extraction from Environment

(defun pel-ffind-env-tool-names (lang)
  "Return a list of language tool chain names extracted from the environment.

The LANG argument is a symbol representing the programming or markup
language, something like c, c++, perl, rst, etc...

Extract the list of tool names from the environment variable named
PEL_DEV_TOOLS_FOR_\\='LANG\\=' (where \\='LANG\\=' corresponds to the
language used by the current buffer or specified FILENAME such as C,
PERL, etc).  The value of the variable environment variable is a
colon-separated list of tool names.

Return a list of tool name strings if there any.  Return nil if the
environment variable does not exists or if it exists but does not
specify any tool names."
  ;; Translate symbols that cannot be used in variable names, like c++.
  ;; Extract the list of tool names from the appropriate environment variable.
  ;; Remove any duplicate, leave nothing empty.
  ;; Trim white space from the beginning or end of any tool name.
  (pel-envvar-value-list (format "PEL_DEV_TOOLS_FOR_%s"
                                 (upcase
                                  (symbol-name
                                   (cond
                                    ((eq lang 'c++) 'cpp)
                                    (t  lang)))))
                         ":"))

;; ---------------------------------------------------------------------------
;;* Identify directories and directory-trees to search
;;  =================================================
;;
;; The directories and directory trees to search are identified by the data
;; stored in the `pel-dev-projects', `pel-dev-tools' and `pel-dev-libraries'
;; user-options that apply to the project corresponding to the currently
;; visited file and its programming language.
;;
;; The project is identified by the visited file and the project directory
;; root that owns the file for the programming language.  Each
;; project/language identified in `pel-dev-projects' may have a set of:
;; - directory-trees identified directly,,
;; - list of directories identified by PATH-like environment variables,
;; - list of directories and directory trees associated with a dev tool chain
;;   identified in the `pel-dev-tools',
;; - list of directories and directory trees associated with a code library
;;   identified in the `pel-dev-libraries'.
;;
;; The code identifies the absolute path of each directory and directory-tree.
;; The user can use ~ and $VARNAME and ${VARNAME} style variable names inside
;; the directory names.  The name of an environment variable can also be
;; composed with other variables and the value of a environment variable may
;; use other environment variables.  The code must expand everything.
;;
;; The extraction of the directories and directory-trees, fully expanded, is
;; specific to each file.  The resulting values are cached into buffer local
;; variables `pel--ffind-searched-dirs' and `pel--ffind-searched-dir-trees'
;; with a `pel--ffind-search-initialized' flag.

(defun pel--ffind-set-project-dirs (&optional filename refresh)
  "Set directories searched by `pel-ffind' for FILENAME or current buffer.

Set the directories and directory trees searched by `pel-ffind' for a search
performed when editing the specified FILENAME (if one is specified) or in the
current buffer.   The filename or buffer mode identifies the current language
which has an impact on the selection of the directory lists extracted from the
structures of the `pel-dev-projects', `pel-dev-libraries' the `pel-dev-tools'
user-options.

Perform the selection of directories if their cached values have not been set
yet or when a REFRESH operation is requested by the argument.  The cached
lists are `pel--ffind-searched-dirs' and `pel--ffind-searched-dir-trees'."
  (when (or (null pel--ffind-search-initialized)
            refresh)
    (let ((dirs/dir-trees/exc-regexps
           (pel--ffind-project-lang-directories filename)))
      (setq pel--ffind-searched-dirs      (car   dirs/dir-trees/exc-regexps)
            pel--ffind-searched-dir-trees (cadr  dirs/dir-trees/exc-regexps)
            pel--ffind-exclusion-regexps  (caddr dirs/dir-trees/exc-regexps)
            pel--ffind-search-initialized t))))

;; ---------------------------------------------------------------------------
;;* Finding file in language-specific project defined in `pel-dev-projects'
;;  =======================================================================


;;-pel-autoload
(defun pel-ffind (fname &optional tree-dpaths inclusive no-ignore)
  "Search for FNAME in current and/or specified TREE-DPATHS.

Search for FNAME file in the specified directories and directory trees.
- If TREE-DPATHS is specified and INCLUSIVE is nil search exclusively
  -  in the directory trees specified by TREE-DPATH.
- If TREE-DPATHS is specified and INCLUSIVE is non-nil search:
  - in the directory trees specified by TREE-DPATH and
  - in the directories and directory trees identified the `pel-dev-projects'
    corresponding to the language specific project for to the currently
    visited file.
- If TREE-DPATHS is nil, then search
  - in the directories and directory trees identified the `pel-dev-projects'
    corresponding to the language specific project for to the currently
    visited file.

The function excludes absolute file names that match one of the regular
expressions returned by `pel-dev-project.setting.exclude-regexps' unless
NO-IGNORE is non-nil.

TREE-DPATHS may be a string (a single directory path) or a list of
strings identifying several directories.

Returns a list of strings, each string is the fully expanded absolute
path of a file found.

The directory tree file searching operation is performed using shell
command identified by `pel-ffind-executable'.

Note that the \\='VCS ignore capability\\=' of fd is not used, so all
files are found whether the VCS ignore (via file like \.gitignore) is
told to ignore them or not."
  (let ((dirs nil)
        (dir-trees nil)
        (exclusion-regexps nil)
        (found-files nil)
        (fpath nil))
    (if (and tree-dpaths
             (not inclusive))
        ;; Search in specified directory trees only.
        (setq dir-trees (pel-list-of tree-dpaths))
      ;; Search in current directory tree and directories and directory trees
      ;; specified by the `pel-dev-project' data structure data associated to
      ;; the current project for the language used by the current buffer.
      ;; - Set the buffer-specific dirs/dir-trees if that has not already been
      ;;   done.
      (pel--ffind-set-project-dirs)
      (setq dirs      pel--ffind-searched-dirs
            dir-trees (cons default-directory pel--ffind-searched-dir-trees)
            exclusion-regexps pel--ffind-exclusion-regexps)
      (when (and tree-dpaths inclusive)
        (dolist (dtree tree-dpaths)
          (unless (member dtree dir-trees)
            (push dtree dir-trees)))))
    ;; Search for FNAME into directory trees first.
    (setq found-files (split-string
                       (string-trim (shell-command-to-string
                                     (pel--ffind-command fname dir-trees)))
                       "\n" t))
    ;; Then search for FNAME into the directories starting with the last
    ;; directory first: this places file found in the first directory at the
    ;; beginning of the list of found files and the files found in directory
    ;; trees toward the end of the list.
    (dolist (dir (reverse dirs))
      (setq fpath (expand-file-name fname dir))
      (when (file-exists-p fpath)
        (push fpath found-files)))
    ;; Remove any duplicate file name from the list
    (setq found-files (seq-uniq found-files #'string=))

    ;; When fname has a directory portion it is ignored in the search
    ;; command created by pel--ffind-command (otherwise the find or fd search
    ;; fails).  The result might include files that are not inside the
    ;; specified directory then.  Remove these files from the result.
    (when (and found-files
               (file-name-directory fname))
      (let ((dir-portion (file-name-directory fname)))
        (setq found-files
              (seq-filter (lambda (fn)
                            (string-match-p (regexp-quote dir-portion) fn))
                          found-files))))
    ;; Unless NO-IGNORE is specified (non-nil) remove any file that match one
    ;; of the regular expression identified in the `pel-dev-project' and
    ;; returned by `pel-dev-project.setting.exclude-regexps'.
    (unless no-ignore
      (dolist (exclude-regxp exclusion-regexps)
        (setq found-files
              (seq-filter (lambda (fn)
                            (not (string-match-p exclude-regxp fn)) )
                          found-files))))
    found-files))

;; ---------------------------------------------------------------------------
;;* Finding file in simple project
;;  ==============================

;;-pel-autoload
(defun pel-generic-find-file (fname &optional tree-dpaths)
  "Find FNAME file name(s) from in project holding the currently visited file.

Recursively search for FNAME inside the directory tree of the current
project, and if specified, inside the list of directory trees specified
by the TREE-DPATHS argument, which may be a single directory path or a
list of directory paths.

Return a list of found file names with complete and expanded absolute path.
Return nil if nothing found."
  (let* ((candidate-dir (pel-ffind-project-rootdir))
         (tree-dpaths (pel-list-of tree-dpaths))
         (searched-directories (if candidate-dir
                                   (cons candidate-dir tree-dpaths)
                                 tree-dpaths))
         (uniq-searched-dirs (delete-dups searched-directories)))
    (pel-ffind fname uniq-searched-dirs)))

;; ---------------------------------------------------------------------------
;; TODO: when there are no directories/directory-trees to search do we search
;; in current directory as a directory or as a directory tree?



;;-pel-autoload
(defun pel-ffind-show-status (&optional append)
  "Show user-options used to control the file finding of current major mode."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (user-buffer-major-mode major-mode)
        (project-name    (pel-ffind-project-name))
        (project-root    (pel-ffind-project-rootdir))
        (tool-names      (pel-ffind-project-lang-tools))
        (var-names       (pel-ffind-project-lang-envvars))
        (exclude-regexps (pel-ffind-project-lang-exclude-regexps))
        (env-tool-names (pel-ffind-env-tool-names (pel-language-of)))
        (dir/trees/exclud-regxp (pel--ffind-project-lang-directories)))
    (pel-print-in-buffer
     "*pel-ffind-status*"
     "PEL FFIND Control Status"
     (lambda ()
       "Print user options & buffer local variables of file finding."
       (pel-insert-bold "\nUser-options:")
       (pel-insert-symbol-content-line 'pel-ffind-executable)
       (pel-insert-symbol-content-line 'pel-project-root-identifiers)
       (pel-insert-symbol-content-line 'pel-project-restricted-root-identifiers)
       (pel-insert-symbol-content-line 'pel-open-file-at-point-dir-home)
       (pel-insert-symbol-content-line 'pel-use-find-file-in-project)
       (insert "\n\nProject/Language file search control:")
       (insert "\n A project has a name and is identified by a root directory.")
       (insert "\n It may support several set of languages,")
       (insert "\n with directories, directory-trees, tools, libraries,")
       (insert "\n PATH-like environment variables and exclusion regexps for each.")
       (pel-insert-list-content 'pel-dev-projects)
       (insert "\n\nA project may refer to directories associated with tool chains identified here:")
       (pel-insert-list-content 'pel-dev-tools)
       (insert "\n\nA project may refer to directories associated with libraries identified here:")
       (pel-insert-list-content 'pel-dev-libraries)

       (pel-insert-bold "\n\nGlobal control variables")
       (insert ", actual values used, identifying file search tool:")
       (pel-insert-symbol-content-line 'pel--ffind-executable)
       (pel-insert-symbol-content-line 'pel--ffind-path)
       (pel-insert-bold "\n\nBuffer local cached control values:")
       (pel-insert-symbol-content-line  'pel--ffind-search-initialized)
       (pel-insert-symbol-content-line  'pel--ffind-overriding-toolchain-name)
       (pel-insert-list-content 'pel--ffind-searched-dirs nil nil nil t)
       (pel-insert-list-content 'pel--ffind-searched-dir-trees nil nil nil t)
       (pel-insert-list-content 'pel--ffind-exclusion-regexps nil nil nil t)


       (pel-insert-bold (format "\n\nActive settings used for this %s buffer:"
                                user-buffer-major-mode))
       (insert (format "\n- Project name        : %s" (if project-name
                                                          project-name
                                                        "not set")))
       (insert (format "\n- Root directory      : %s" (if project-root
                                                          project-root
                                                        "none")))

       (pel-insert-list-value "Exclude regexps          " exclude-regexps t t)
       (pel-insert-list-value "Project tool chains      " tool-names t t)
       (pel-insert-list-value "Project env tool chains  " env-tool-names t t)
       (pel-insert-list-value "PATH-like env variables  " var-names t t)
       (insert "\n")
       (pel-insert-list-value "Searched directories     "
                              (car dir/trees/exclud-regxp) nil t)
       (pel-insert-list-value "Searched directory trees "
                              (cadr dir/trees/exclud-regxp) nil t))

     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind)

;;; pel-ffind.el ends here

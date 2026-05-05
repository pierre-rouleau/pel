;;; pel--install.el --- PEL file and package installation and lazy loading.  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 12 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-05 15:46:46 EDT, updated by Pierre Rouleau>

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
;; Lazy loading and package installation:
;; - `pel-install-github-file'
;; - `pel-install-github-files'
;;   - `pel--install-github-files'
;;     - `pel-install-files'
;;       - `pel-install-file'
;;         - `pel-url-copy-file'
;;
;; - `pel-soft-require-or-warn'
;; - `pel-require'
;;   - `pel-package-installed-p'
;;   - `pel-package-install'
;;   - `pel--require-warn'
;;
;; - `pel-ensure-package-elpa'
;;   - `pel--ensure-pkg-elpa'
;;     - `pel--pin-package'
;;       - `pel-archive-exists'
;;    - `pel--package-ensure-elpa'
;;      - `pel--package-install-elpa'
;;

;; Lazy loading and package installation:
;;
;; The first set of functions and macros provide mechanism to require, load,
;; autoload and byte-compiler declaration facilities.
;;
;;
;; @ `pel-require-at-load'
;;   - `pel--require-at-load'
;; @ `pel-require-after-init'
;;   - `pel--require-after-init'
;; @ `pel-eval-after-load'
;; @ `pel-set-auto-mode'
;; @ `pel-autoload-file'
;; @ `pel-declare-file'

;; Speedbar Support
;; - `pel-add-speedbar-extension'
;;
;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
(require 'pel--indent)           ; use: `pel-tab-width-control-variables'.
(require 'pel--options)          ; use: `pel-use-tree-sitter'
(eval-when-compile
  (require 'pel--macros))        ; use `pel-append-to' to generate code.

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Package and file installation and lazy loading
;;  ==============================================
;;
;; The first set install files downloaded from the internet with a specific
;; URL inside PEL utils directory.
;; These functions do not depend on Emacs package facility.  Therefore they
;; can be used any time, including when PEL operates in fast-startup mode.
;;
;; - `pel-install-file'  downloads and installs one file.
;; - `pel-install-files' downloads and installs one or several files from the
;;   same web site.
;;
;; -> `pel-install-files'
;;    -> `pel-install-file'
;;       - `pel-url-copy-file'

;; The next set of functions does the same thing but provide logic
;; specific to build GitHub or Gitlab URLs.
;; They install the files inside PEL utils directory.
;; These functions do not depend on Emacs package facility.  Therefore they
;; can be used any time, including when PEL operates in fast-startup mode.
;;
;; - `pel-install-github-files' downloads and installs one or several files
;;    from GitHub specified user project branch.
;; - `pel-install-github-file' downloads and installs one file.  That file
;;   may have a name that differs from the URL used to download it.  This is
;;   mostly used when a file name has a character that cannot be part of a URL
;;   and must be encoded differently.
;; - `pel-install-gitlab-file' downloads, installs and compile one file from
;;   Gitlab.
;;
;; -> `pel-install-github-file'
;;     . `pel-install-file'
;; -> `pel-install-github-files'
;;     . `pel-install-files'
;;       . `pel-install-file'
;; -> `pel-install-gitlab-file'
;;     . `pel-install-file'


;; The next set of functions and macros provide logic to install Elpa
;; compliant packages when PEL is not running in fast-startup mode
;; and to require Emacs packages.
;;
;; -> - `pel-require'
;;      - `pel-package-installed-p'
;;      - `pel-package-install'
;;        `pel-install-github-file'

;; -> @ `pel-ensure-package-elpa'
;;      - `pel--ensure-pkg-elpa'
;;        - `pel--pin-package'
;;          - `pel-archive-exists'
;;       - `pel--package-ensure-elpa'
;;         - `pel--package-install-elpa'
;;

;; The next set of functions and macros provide logic to install
;; packages via quelpa.  This allows installation of multi-file packages
;; inside the elpa directory as if they were elpa-compliant.
;; However, they install nothing when PEL runs in fast-startup mode.
;;
;; -> @ `pel-quelpa-install'
;;      - `pel--quelpa-install'

;;  The next set of macros, defined in pel--keys.macros.el, control the
;;  loading and evaluation of features and code, mostly used in pel_keys.el
;;
;; @ `pel-require-at-load'
;;   - `pel--require-at-load'
;; @ `pel-require-after-init'
;;   - `pel--require-after-init'
;; @ `pel-eval-after-load'
;; @ `pel-set-auto-mode'
;; @ `pel-autoload-file'
;; @ `pel-declare-file'

;; [:todo 2026-03-09, by Pierre Rouleau: Modify all functions that download
;;                    and install files to return non-nil on success, nil on error to allow
;;                    pel_keys.el code to proceed only when the file is either present or just
;;                    downloaded.  Ideally the functions would return 'present or 'downloaded and
;;                    the pel_keys.el code would not map commands when the command failed and the
;;                    file is not installed locally.  There would not be any exception from
;;                    failing installation just error warnings displayed describing what went
;;                    wrong.  This way problems would not stop Emacs initialization.  For the
;;                    moment coding issues or permission failures may stop the
;;                    initialization.
;;                    However do this only once the fast startup works on all
;;                    version of Emacs as the extra code will slow down
;;                    startup a little.]


(defun pel-isa-http-404-error-p (&optional buffer)
  "Return t if the BUFFER content is a 404 HTTP status error, nil otherwise."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (forward-line 4)
      (let ((limit (point)))
        (goto-char (point-min))
        (pel-as-boolean
         (re-search-forward
          "\
\\b404\\b\\(?:\\.[[:digit:]][[:digit:]]?\\)?[[:space:]]?\
\\(?::?[[:space:]]*Not Found\\b\\)"
          limit
          'noerror))))))

(defun pel-url-copy-file (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.  Both arguments must be strings.

Same as `url-copy-file' but detects URL to non-existing file reported as a
HTTP 404 error by the server.

If the NEWNAME file already exists, download it again when
OK-IF-ALREADY-EXISTS is non-nil otherwise treats this as an error.

On success return NEWNAME, the name of the created file.
On operation error, display a descriptive :error warning message and return
nil.  Raise an error if the `url-copy-file' is not bound.
That should never happen if Emacs is installed properly."
  (require 'url-handlers nil 'noerror)
  (if (fboundp 'url-copy-file)
      ;; Try to download the file identified by the URL.
      ;; That function does not detect invalid URLS so we could get a "404:
      ;; Not Found"
      (let ((tmp-fname (make-temp-file "pel-url-copy-file"))
            (error-msg nil))
        (unwind-protect
            (condition-case err
                ;; `url-copy-file' does not complain when the server replies
                ;; with a "404: Not Found"; it simply stores it inside the
                ;; created file.
                (if (url-copy-file url tmp-fname t)
                    ;; Check that the file was properly downloaded by checking
                    ;; if its content is "404: Not Found".  If it is: set
                    ;; error-msg with a descriptive problem.
                    (progn
                      (with-temp-buffer
                        (insert-file-contents tmp-fname)
                        (when (pel-isa-http-404-error-p (current-buffer))
                          (setq error-msg
                                (format
                                 "Received 404 error for requested URL: %s"
                                 url))))
                      (unless error-msg
                        (copy-file tmp-fname newname
                                   ;; Prevent prompt if `ok-if-already-exists'
                                   ;; was passed a number.
                                   (pel-as-boolean ok-if-already-exists))))
                  (setq error-msg (format "Nothing received for URL: %s" url)))
              (error
               (setq error-msg
                     (format "Exception detected in url-copy-file: %s %s"
                             (car err)
                             (cdr err)))))
          (when (file-exists-p tmp-fname)
            (delete-file tmp-fname)))
        ;; After operation check if there was any error reported.
        ;; On success return the name of the created file.
        ;; On error: display an :error warning and return nil.
        (if error-msg
            (progn
              (display-warning 'pel-url-copy-file
                               (format "Error installing URL %s to %s:\n%s"
                                       url newname
                                       error-msg)
                               :error)
              nil)
          ;; success: return name of created file.
          newname))
    ;; url-copy-file is not bound
    (error "\
url-handlers.el `url-copy-file' not bound in pel-url-copy-file.\
  Can't install anything!")))

(defun pel-install-file (url fname &optional refresh)
  "Download, install a file FNAME from URL into PEL\\='s utility directory.
On success, byte compile that file and when Emacs use native compilation
then also build the native-compiled .eln file for it.

The utility directory is the \\='utils\\=' sub-directory of the Emacs
directory identified by the Emacs variable `user-emacs-directory'.
If this directory does not exist, the function creates it.

If the file already exists in the destination, no download
is done unless REFRESH is non-nil.

Returns non-nil when file was downloaded, nil otherwise.
Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (let* ((utils-dirname (file-name-as-directory
                         (expand-file-name "utils" user-emacs-directory)))
         (target-fname (expand-file-name fname utils-dirname))
         (subdir (file-name-directory fname))
         (downloaded nil))
    (if (file-in-directory-p target-fname utils-dirname)
        (progn
          ;; create utils directory and sub-directory if required
          (unless (file-exists-p utils-dirname)
            (make-directory utils-dirname 'make-parents-if-needed))
          (when subdir
            (setq subdir (expand-file-name subdir utils-dirname))
            (unless (file-exists-p subdir)
              (make-directory subdir 'make-parents-if-needed)))

          (when (or (not (file-exists-p target-fname))
                    refresh)
            (message "Downloading %s" url)
            (setq downloaded (pel-url-copy-file url target-fname refresh))
            (when (and downloaded
                       (equal (file-name-extension target-fname) "el"))
              (message "Byte compiling it to %s" target-fname)
              (byte-compile-file target-fname)
              (when (and (fboundp 'native-comp-available-p)
                         (fboundp 'native-compile-async)
                         (native-comp-available-p))
                (native-compile-async target-fname)))))
      (display-warning 'pel-install-file
                       (format "\
Cannot install %s inside PEL utils: it would be stored outside utils!\n\
Fix the file specification in pel_keys.el!" fname)
                       :error))
    downloaded))

(defun pel-install-files (url-base fnames &optional refresh)
  "Download & install files identified by their URL-BASE and FNAMES.

The URL-BASE is the common URL for the location of all files.

The FNAMES is a file name string or list of file name strings
identifying the name of the file located at that URL-BASE and
also the name of the file save locally into the PEL Emacs \\='utils\\='
directory.  See `pel-install-file' for more info.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil.

Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (dolist (fname (pel-list-of fnames))
    (pel-install-file (pel-url-join url-base fname)
                      fname
                      refresh)))

;; -------

(defun pel-install-github-files (user-project-branch
                                 fnames
                                 &optional refresh)
  "Download & install FNAMES from GitHub USER-PROJECT-BRANCH.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAMES is a file name string or list of file names.

If a file already exists in the destination, no download is done
unless REFRESH is non-nil.

Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (pel-install-files (pel-url-join "https://raw.githubusercontent.com"
                                   user-project-branch)
                     fnames
                     refresh))



(defun pel-install-github-file (user-project-branch
                                fname
                                &optional url-fname refresh)
  "Download & install FNAME from GitHub USER-PROJECT-BRANCH/URL-FNAME.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAME is the name of the file, with its .el extension.
- URL-FNAME is the name of the file as it appears in the
  URL. This argument is only required when it differs from FNAME.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil.

The function returns t if the file was downloaded, nil otherwise.
Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (pel-install-file (pel-url-join "https://raw.githubusercontent.com"
                                  user-project-branch
                                  (or url-fname fname))
                    fname
                    refresh))

;; --

(defun pel-install-gitlab-file (gitlab-user gitlab-project fname
                                            &optional branch refresh)
  "Download & install FNAME from Gitlab user and project into PEL utils.

GITLAB-USER is the name of Gitlab user.
GITLAB-PROJECT is the name of Gitlab project.
BRANCH is optional: it identifies the repo branch, and is \"master\" if not
specified.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil.

The function returns t if the file was downloaded, nil otherwise.
Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (pel-install-file (format "https://gitlab.com/%s/%s/-/raw/%s/%s"
                            gitlab-user
                            gitlab-project
                            (or branch "master")
                            fname)
                    fname
                    refresh))

;; -------
(defun pel-package-install (pkg)
  "Install package PKG, return t on success, nil otherwise.

PKG must be a symbol naming one of the available packages in one
of the archives listed in variable `package-archives'.

If the first attempt fails, the function refreshes the package
list and tries again.  This prevents failing to install a package
when its version identified in the package list identifies an
obsolete version no longer supported by the Elpa archive site.

If the second attempt fails, then a error-level warning is logged
and the function returns nil"
  ;; package.el is part of Emacs but it's not loaded until required.
  ;; Load it lazily and check if the required functions are bound
  ;; to prevent byte-compiler warnings.
  (let ((package-was-installed nil))
    (if (and (require 'package nil 'noerror)
             (fboundp 'package-install))
        (condition-case-unless-debug err
            (progn
              (package-install pkg)
              (setq package-was-installed t))
          (error
           (if (and (fboundp 'package-refresh-contents)
                    (fboundp 'package-read-all-archive-contents)
                    (boundp  'package-pinned-packages))
               (progn
                 (message (format "Failed to install %s: %s
  Refreshing package list and re-trying..."
                                  pkg
                                  (error-message-string err)))
                 (package-refresh-contents)
                 (condition-case-unless-debug err
                     (progn
                       (when (assoc pkg
                                    (bound-and-true-p package-pinned-packages))
                         (package-read-all-archive-contents))
                       (package-install pkg)
                       (setq package-was-installed t))
                   (error
                    (display-warning
                     'pel-package-install
                     (format "After refresh, failed to install %s: %s"
                             pkg
                             (error-message-string err))
                     :error))))
             (display-warning
              'pel-package-install
              (format "The package.el is not loaded properly.
Failed installation of %s.
Please verify the validity of your package-archives setup!"
                      pkg)
              :error))))
      (display-warning
       'pel-package-install
       (format  "package-install is void. Can't install %s!
Please verify the validity of your package-archives setup!"
                pkg)
       :error))
    package-was-installed))

(defun pel-package-installed-p (feature)
  "Return t if FEATURE is installed, nil otherwise.
Load the package library if that's not already done."
  (if (and (require 'package nil 'noerror)
           (fboundp 'package-installed-p))
      (package-installed-p feature)
    (display-warning 'pel-package-installed-p
                     "Failed loading package.el to use package-installed-p!"
                     :error)
    nil))

(defmacro pel-soft-require-or-warn (feature &rest body )
  "Soft require FEATURE (unquoted symbol), display warning on failure.
if BODY is specified, execute it on success."
  (declare (indent 1))
  (let ((warning-name (intern (format "pel-use-%s" feature)))
        (warning-text (format "Can't load %s; skipping." feature)))
    (if body
        `(if (require (quote ,feature) nil 'noerror)
             (progn
               ,@body)
           (display-warning (quote ,warning-name)
                          ,warning-text
                          :error))
      `(unless (require (quote ,feature) nil 'noerror)
         (display-warning (quote ,warning-name)
                          ,warning-text
                          :error)))))

(defun pel--require-warn (message)
  "Utility - display warning with MESSAGE in `pel-require'."
  (display-warning 'pel-require message :warning))

(defun pel-require (feature &optional package with-pel-install
                            fname url-fname)
  "Load FEATURE if not already loaded, optionally try to install PACKAGE.

If the FEATURE is not already loaded, require it.  If that fails,
then attempt to install the package if requested by the arguments and try
require the feature again.

FEATURE: a symbol, the feature to load if not already loaded.
PACKAGE: one of the following:
- nil:                      If not loaded, don't attempt to install; simply
                            display a warning that the feature is not loaded.
- `:install-when-missing':  If not loaded attempt to install a package with
                            the same name as the feature.
- any other symbol:         If FEATURE is not loaded,  attempt to install the
                            package with the PACKAGE name.

If FEATURE is not loaded and a package must be installed (as specified by
PACKAGE argument), WITH-PEL-INSTALL describes how to install the package:
- nil:       Install PACKAGE with `pel-package-install'.
- a string:  Install PACKAGE with `pel-install-github-file'.  In that case,
             WITH-PEL-INSTALL must be the USER-PROJECT_BRANCH, and
             the FNAME is the name of the .el file and URL-FNAME is the
             explicit file URL if needed.
             All 3 are passed to `pel-install-github-file'.

Generate a warning when failing to load the FEATURE, skipping requested
installation due to running in fast startup mode or failing to install
package.

Return the loading state of the FEATURE."
  (unless (featurep feature)
    (let ((feature-is-loaded (require feature nil 'noerror))
          (try-final-load nil)
          (install-failed nil))
      (unless feature-is-loaded
        ;; required failed
        (if package
            (if (pel-in-fast-startup-p)
                ;; in fast startup don't attempt to install anything.
                (pel--require-warn
                 (format
                  "%s not loaded, but skip installing %s during fast startup."
                  feature package))
              ;; in normal mode attempt to install package if requested
              (let ((package-to-install (if (eq package :install-when-missing)
                                            feature
                                          package)))
                (if with-pel-install
                    ;; install using specified GitHub repository
                    (progn
                      (if (pel-install-github-file with-pel-install
                                                   fname url-fname)
                          (setq try-final-load t)
                        (setq install-failed t)))
                  ;; install an elpa-compliant package if not already present
                  (if (pel-package-installed-p package-to-install)
                      (progn
                        (pel--require-warn
                         (format
                          "Failed loading %s (but package %s is installed!)"
                          feature package-to-install)))
                    (if (pel-package-install package-to-install)
                        (setq try-final-load t)
                      (setq install-failed t))))
                (when install-failed
                  (pel--require-warn
                   (format "%s load failed. Tried installing %s also failed."
                           feature package-to-install)))
                (when try-final-load
                  (require feature nil 'noerror)
                  (unless (featurep feature)
                    (pel--require-warn
                     (format
                      "Failed loading %s even after installing package %s!"
                      feature package-to-install))))))
          (pel--require-warn
           (format "Failed loading %s.  No install requested." feature))))))
  (featurep feature))

;; ---------------------------------------------------------------------------
;;
;; The following code defines the `pel-ensure-package-elpa' macro that PEL
;; uses to install Elpa-compliant packages.
;;
;; This is done to:
;; - Install a package when the appropriate pel-use variable is turned on.
;; - Does NOT install when byte-compiling the code.
;; - Does NOT install when PEL is operating in fast startup mode.
;; - Allow the selection of a Elpa site, just as the use-package :pin does.
;;
;; The `pel-ensure-package-elpa' macro uses the `pel--ensure-pkg-elpa'
;; function to reduce the amount of code generated and executed to the expense
;; of one function call.
;;
;; Credit: the package installation code was influenced by the
;; use-package library found at https://github.com/jwiegley/use-package
;; and now part of Emacs.
;;
;; PEL does not use the use-package library in attempt to reduce the overhead
;; and the startup time further.


(defun pel-archive-exists (archive)
  "Return non-nil if specified package ARCHIVE is being used, nil otherwise.
The ARCHIVE argument may be a string or a symbol.
To get the URL of the existing package, take the cdr of the returned value."
  (if (or (boundp 'package-archives)
          (and (require 'package nil 'noerror)
               (boundp 'package-archives)))
      (with-no-warnings ; Emacs 30 Byte compiler does not see protection...
        (assoc (pel-as-string archive) package-archives))
    (display-warning 'pel-archive-exists
                     "package.el is not loaded: package-archives is void"
                     :error)
    nil))

(defvar pel--pinned-packages nil
  "List of packages that are associated with a specific Elpa archive.")
(defvar package-pinned-packages) ; prevent warning when accessing package var.

(defun pel--pin-package (package archive)
  "Pin PACKAGE (a symbol) to ARCHIVE (a symbol or string)."
  (let ((archive-name (pel-as-string archive)))
    (if (pel-archive-exists archive-name)
        (progn
          (add-to-list 'pel--pinned-packages
                       (cons package (pel-as-string archive)))
          (add-to-list 'package-pinned-packages
                       (cons package (pel-as-string archive))))
      (error "\
Archive '%S' requested for package '%S' is not listed in package-archives!"
             archive package)))
  (unless (bound-and-true-p package--initialized)
    (package-initialize t)))

(defun pel--package-install-elpa (package)
  "Install PACKAGE (a symbol).  On failure retry once and issue an error.

Packages in the Elpa archive sites are regularly updated and old
versions purged.  Requesting an old version of a package may
occur when our local list is outdated.

When a failure occurs, refresh the local list and try again, also
generate a warning that identifies the error."
  (declare-function package-install                   "package")
  (declare-function package-refresh-contents          "package")
  (declare-function package-read-all-archive-contents "package")
  (defvar package-archive-contents)
  ;;
  (condition-case-unless-debug err
      (package-install package)
    (error
     (message "Error trying to install %s : %s.  \
Refreshing package list and trying again." package err)
     (package-refresh-contents)
     (package-read-all-archive-contents)
     (if (assoc package package-archive-contents)
         (package-install package)
       (display-warning 'pel--install-package
                        (format "Failed locating package %s" package)
                        :error)))))

(defun pel--package-ensure-elpa (package)
  "Install specified Emacs Lisp PACKAGE (a symbol).

DO NOT use this function directly inside your code.
Use the macro `pel-ensure-package-elpa' instead.

When a failure occurs, refresh the local list and try again, also
generate a warning that identifies the error."
  (if (and (require 'package nil 'noerror)
           (boundp 'package-archive-contents)
           (fboundp 'package-read-all-archive-contents))
      (condition-case-unless-debug err
          (progn
            (when (assoc package (bound-and-true-p
                                  pel--pinned-packages))
              (package-read-all-archive-contents))
            (if (assoc package package-archive-contents)
                (pel--package-install-elpa package)
              (package-refresh-contents)
              (when (assoc package (bound-and-true-p
                                    pel--pinned-packages))
                (package-read-all-archive-contents))
              (pel--package-install-elpa package))
            t)
        (error
         (display-warning 'pel-ensure-package-elpa
                          (format "Failed trying to install %s: %s"
                                  package (error-message-string err))
                          :error)))
    (display-warning 'pel-ensure-package-elpa
                     (format
                      "Cannot install %s: package.el is not properly loaded."
                      package)
                     :error)))

(defun pel--ensure-pkg-elpa (pkg &optional elpa-site)
  "Install package PKG (a symbol) possibly from pinned ELPA-SITE.

If ELPA-SITE is non-nil it should be a symbol or string holding the name
of one of the Elpa repositories identified in the variable
`package-archives'.

When PEL operates in fast startup, nothing is done."
  (unless (pel-in-fast-startup-p)
    (when elpa-site
      (pel--pin-package pkg elpa-site))
    (pel--package-ensure-elpa pkg)))

(defmacro pel-ensure-package-elpa (pkg &optional from: pinned-site)
  "Install package named PKG, optionally from specified PINNED-SITE.
PKG must be an unquoted symbol.
FROM: is just a tag.
When PINNED-SITE (a unquoted symbol) is specified use this as the Elpa
repository, which must be listed in the variable `package-archives'.

The FROM: argument must be present.  It is cosmetics only.

The package list is refreshed before attempting installation to
prevent trying to install an obsolete version of a package that
is no longer present on the Elpa site.
When a failure occurs, refresh the local list and try again, also
generate a warning that identifies the error.

However, when PEL operates in fast startup, the macro creates no code."
  (declare (indent 1))
  (ignore from:)
  (let ((pin-site-name (when pinned-site (symbol-name pinned-site))))
    `(unless (or (pel-in-fast-startup-p)
                 (pel-package-installed-p (quote ,pkg)))
       (pel--ensure-pkg-elpa (quote ,pkg) ,pin-site-name))))

;; -------

(defun pel--quelpa-install (package quelpa-specs)
  "Install PACKAGE using specified QUELPA-SPECS.
Don't install it if already installed."
  (unless (or (pel-in-fast-startup-p)
              (pel-package-installed-p package))
    (if (fboundp 'quelpa)
        (quelpa quelpa-specs)
      (display-warning
       'pel-quelpa-install
       (format "Please activate pel-use-quelpa to install %S"
               quelpa-specs)))))

(defmacro pel-quelpa-install (quelpa-specs)
  "Install the package identified by QUELPA-SPECS.
QUELPA-SPECS is an unquoted form that identifies the package
to install and how to install it.  See `quelpa' documentation.
Don't install it if already installed or PEL in fast startup."
  (declare (indent 1))
  (if (listp quelpa-specs)
      (let ((package (car quelpa-specs)))
        (if (symbolp package)
            `(pel--quelpa-install (quote ,package) (quote (,@quelpa-specs)))
          (byte-compile-warn
           "Invalid quelpa-spec: first element not a symbol: %S"
           package)))
    (byte-compile-warn "Invalid quelpa-specs: %S" quelpa-specs)
    nil))

;; ---------------------------------------------------------------------------
;; Delay activation of Modes after processing of command line arguments
;; --------------------------------------------------------------------
(eval-and-compile
  (defmacro pel-after-startup-do (&rest body)
    "Schedule BODY execution after processing of command line arguments."
    `(add-hook 'emacs-startup-hook
               (lambda ()
                 ,@body)
               :append)))
;; ---------------------------------------------------------------------------
(defun pel--require-at-load (feature)
  "Require specified FEATURE when loading only, not when compiling.
FEATURE must be a quoted symbol.
This is normally used by the macro `pel-require-at-load'."
  (unless (require feature nil :noerror)
    (display-warning 'pel-require-at-load
                     (format "Failed loading %s" feature)
                     :error)))

(defmacro pel-require-at-load (feature)
  "Require specified FEATURE when loading only, not when compiling.

FEATURE must be an unquoted symbol representing the required
feature."
  `(cl-eval-when 'load
     (pel--require-at-load (quote ,feature))))

;; --
(defun pel--require-after-init (feature secs)
  "Require specified FEATURE some SECS after initializing Emacs.
FEATURE must be a quoted symbol.
This is normally used by the macro `pel-require-after-init'."
  (run-with-idle-timer secs nil
                       (function require)
                       feature nil :noerror))

(defmacro pel-require-after-init (feature secs)
  "Require specified FEATURE some SECS after initializing Emacs.

Don't require the feature when compiling.
FEATURE must be an unquoted symbol representing the required
feature.
SECS may be an integer, a floating point number, or the internal
time format returned by, e.g., `current-idle-time’."
  `(cl-eval-when 'load
     (pel--require-after-init (quote ,feature) ,secs)))


;; --
(defmacro pel-set-auto-mode (mode for: &rest regexps)
  "Activate automatic MODE for the list of file REGXEPS.
The FOR: argument is a cosmetic separator.
MODE must be an un-quoted symbol.
FOR: separator must be present.  It is cosmetic only.
REGEXPS is on or several regular expression strings."
  (declare (indent 0))
  (ignore for:)
  (let ((forms ()))
    (setq forms
          (dolist (regxp regexps (nreverse forms))
            (push `(add-to-list 'auto-mode-alist
                                (quote (,regxp . ,mode)))
                  forms)))
    `(progn
       ,@forms)))

;; --

(defmacro pel-autoload-file (fname for: &rest commands)
  "Schedule the autoloading of FNAME for specified COMMANDS.
FNAME is either a string or an unquoted symbol.
The autoload is generated only when the command is not already bound.
Argument FOR: just a required separator keyword to make code look better.

The macro also generates a `declare-function' for each function in
COMMANDS preventing byte-compiler warnings on code referencing these
functions."
  (declare (indent 0))
  (ignore for:)
  (let ((fname     (if (stringp fname) fname (symbol-name fname)))
        (decl-fcts ()))
    (dolist (fct commands)
      (push `(declare-function ,fct ,fname) decl-fcts))
    (if (> (length commands) 1)
        `(progn
           (dolist (fct (quote (,@commands)))
             (unless (fboundp fct)
               (autoload fct ,fname nil :interactive)))
           ,@decl-fcts)
      `(progn
         (unless (fboundp (quote ,@commands))
           (autoload (quote ,@commands) ,fname nil :interactive))
         ,@decl-fcts))))

;; --
(defmacro pel-declare-file (fname defines: &rest commands)
  "Declare one or several COMMANDS to be defined in specified FNAME.
This does not generate any code.  It prevents byte-compiler warnings.
DEFINES: is a cosmetic only argument that must be present."
  (declare (indent 0))
  (ignore defines:)
  (let ((fname     (if (stringp fname) fname (symbol-name fname)))
        (decl-fcts ()))
    (dolist (fct commands)
      (push `(declare-function ,fct ,fname) decl-fcts))
    `(progn
       ,@decl-fcts)))

;; --

(defun pel--eval-after-load-error (feature error)
  "Display warning for FEATURE loaded by `pel-eval-after-load'.
The ERROR argument is the caught error."
  (display-warning 'pel-eval-after-load
                   (format "Failed configuring %s: %s"
                           feature
                           error)
                   :error))

(defconst pel--ts-mode-with-fixer '(ada-ts-mode
                                    dart-ts-mode
                                    elixir-ts-mode
                                    erlang-ts-mode
                                    go-ts-mode
                                    js-ts-mode
                                    rust-ts-mode
                                    zig-ts-mode)
  "List of Tree Sitter modes that require execution of a mode fixer function.

The fixer mode function has a name that has a format like
pel--MODE-fixer with where MODE corresponds to the name of the mode
taken from this list.")

(defmacro pel-eval-after-load (features &rest body)
  "Evaluate BODY after the FEATURES has been loaded.
FEATURE is either a feature symbol or a list of feature symbols.
Both must be unquoted.
A list of feature symbol is useful, for example, when the tree-sitter
mode is provided by a different file them the classic major mode,
and the tree-sitter mode file does not load the classic mode file."
  (declare (indent 1))
  (let ((code nil)
        (feature-body nil))
    (dolist (the-feature (if (listp features) features (list features)))
      (setq feature-body nil)
      (when (memq the-feature pel--ts-mode-with-fixer)
        (let ((fixer-fct (intern (format "pel--%s-fixer" the-feature))))
          (pel-append-to feature-body
            `((when (fboundp (quote ,fixer-fct))
                (,fixer-fct))))))
      (pel-append-to feature-body
        `((condition-case-unless-debug err
              (progn ,@body)
            (error (pel--eval-after-load-error (quote ,the-feature)
                                               err)))))
      (pel-append-to code
        `((with-eval-after-load (quote ,the-feature)
            ,@feature-body))))
    ;; Return the generated code for all features.
    `(progn
       ,@code)))
;; ---------------------------------------------------------------------------
;; Speedbar Support
;; ----------------

(defun pel-add-speedbar-extension (extension)
  "Add Speedbar support for the specified file EXTENSION.
EXTENSION is either a string or a list of strings.
Each string is either:
- a complete filename,
- a the file extension starting with a (non-quoted) period,
- a regular expression to express the above.

`pel-add-speedbar-extension' is a direct proxy to
`speedbar-add-supported-extension' with the ability to load the
speedbar file."
  (require 'speedbar)
  (declare-function speedbar-add-supported-extension "speedbar")
  (speedbar-add-supported-extension extension))

;; ---------------------------------------------------------------------------
;;* Major Mode Configuration
;;  ========================

(defun pel--mode-hook-maybe-call (fct mode hook &optional append)
  "Schedule FCT as the MODE HOOK: call it if buffer is currently in that MODE.
The function FCT is added at the beginning of the hook list unless the
optional argument APPEND is non-nil, in which case it is added at the end."
  (add-hook hook fct append)
  ;; if the current mode is the required mode also run the specified function
  (if (eq major-mode mode)
      (funcall fct)))

(defconst pel--tab-controlling-major-modes
  '(cwl
    dart      ; dart-mode and dart-ts-mode ; differ; explicit logic is needed.
    go
    go-dot-mod
    go-mod                              ; for go-mod-ts-mode
    ibuffer
    intel-hex
    janet
    js2 js3
    lfe inferior-lfe
    lisp arc clojure
    makefile
    nimscript
    nix
    perl
    scheme chez chibi chicken gambit gerbil guile mit-scheme racket scsh
    seed7
    shell
    ssh-authorized-keys ssh-known-hosts
    term
    tup)
  "List of major mode that fully control the tab behaviour and width.

These modes do not have both `pel-<mode>-tab-width' and a `pel-<mode>-use-tabs'
user-options variables.")

(defun pel-treesit-remap-available-for (mode)
  "Return non-nil when treesit is available the ts MODE can use MODE.
MODE is a symbol like \\='c or \\='rust identifying the major mode."
  (and pel-use-tree-sitter
       (pel-treesit-language-available-p mode)
       (boundp 'major-mode-remap-alist)))

(defun pel--set-indent-control-variables (indent-to-tab-width)
  "Activate the value identified by the INDENT-TO-TAB-WIDTH.
This must be the value of the  pel-MM-tie-indent-to-tab-width customizable
user-option, where MM is the major mode name (like c or python).
The function saves its value in the `pel-tab-width-control-variables' buffer
local variable."
  (let ((value (if (eq indent-to-tab-width 'use-predef-vars)
                   (let ((constvar (pel-major-mode-symbol-for
                                    "pel--%s-indent-predef-vars")))
                     (when (boundp constvar)
                       (symbol-value constvar)))
                 indent-to-tab-width)))
    (setq-local pel-tab-width-control-variables value)))

(defun pel--auto-activate-fly ()
  "Auto-activate fly syntax checking engine if necessary.
Automatic activation is done for a file identified inside
`pel-files-activating-syntax-check' when the major mode is in
`pel-fly-engine-for-modes' and identifies an fly engine."
  (let ((filename (buffer-file-name))
        (found nil)
        (fullpath-name nil)
        (engine nil))
    (when filename
      (setq engine (car-safe
                    (cdr-safe
                     (assoc (intern (pel-file-type-for major-mode))
                            pel-fly-engine-for-modes))))
      (when engine
        ;; [:todo 2025-12-11, by Pierre Rouleau: optimize with a while?]
        (dolist (pathname pel-files-activating-syntax-check)
          (setq fullpath-name (expand-file-name pathname))
          (if (pel-string-ends-with-p pathname "/")
              (when (pel-string-starts-with-p filename fullpath-name)
                (setq found t))
            (when (string= fullpath-name filename)
              (setq found t))))
        (when found
          (cond
           ((eq engine 'flymake) (flymake-mode 1))
           ((eq engine 'flycheck) (when pel-use-flycheck
                                    (with-no-warnings
                                      (flycheck-mode 1))))))))))

;; --

(defun pel-local-set-f12 (prefix &optional key)
  "Assign the <f12> or <f12> KEY to PREFIX."
  (if key
      (local-set-key (kbd (format "<f12> %s" key))   prefix)
    (local-set-key (kbd "<f12>")   prefix)))

(defun pel-local-set-f12-M-f12 (prefix &optional key)
  "Assign the <f12>/<M-f12> or <f12>/<M-f12> KEY to PREFIX."
  ;; Bind the M-F12 first and F12 last so F12 shows up in menu.
  (if key
      (progn
        (local-set-key (kbd (format "<M-f12> %s" key)) prefix)
        (local-set-key (kbd (format "<f12> %s" key))   prefix))
    (local-set-key (kbd "<M-f12>") prefix)
    (local-set-key (kbd "<f12>")   prefix)))

;; --

;; TODO: pel-config-major-mode does not seem to support shell-mode and
;;       term-mode properly.  Investigate and fix.

(defmacro pel-config-major-mode (target-mode key-prefix ts-option &rest body)
  "Setup the major mode identified by TARGET-MODE.

TARGET-MODE is an unquoted symbol identifying the mode: it's the
major mode name without the -mode suffix.  Something like
emacs-lisp, c, python, etc...

The KEY-PREFIX argument is a PEL mode-specific key-prefix unquoted
symbol.  Something like symbol `pel:for-c' and symbol `pel:for-make'.
That symbol must already been defined prior to the macro invocation, and
it should have been defined with a `define-pel-global-prefix' form.  If
KEY-PREFIX is nil or has the value :no-f12-keys then no <f12> and
<M-f12> PEL key prefixes are created for the major mode.

The TS-OPTION control how tree-sitter mode is supported.
This can only be one of the following:
- :no-ts          : no special tree-sitter support

- :ts-only        : support for tree-sitter specific mode only is requested,
                    but no support for the classic mode

- :same-for-ts    : when the tree-sitter-based mode derives from the normal
                    mode and PEL must support both.

- :independent-ts : when the ts-sitter mode exists but does not derive from
                    the normal mode and PEL must support both.

- :same-for-ts-early-remap : It behaves exactly like :same-for-ts for hook
 registration (both the classic mode hook and the *-ts-mode-hook are wired up),
 but it skips the major-mode-remap-alist update inside the hook body because
 the caller has already done it eagerly at init time.

The BODY is a set of forms to execute when the major mode hook
executes, at the moment when a buffer with that major mode opens
and after the local variables have been loaded."
  (declare (indent 3))
  (unless (memq ts-option '(:no-ts :ts-only :same-for-ts :independent-ts
                                   :same-for-ts-early-remap))
    (display-warning 'Invalid-PEL-code
                     (format
                      "Invalid (pel-config-major-mode %S %S %S)"
                      target-mode key-prefix ts-option)))
  (let ((gn-fct1 (intern (format "pel--setup-for-%s-with-local-vars"
                                 target-mode)))
        (gn-docstring1
         (format "\
Activate %s setup, take local variables into account.
Function created by the `pel-config-major-mode' macro."
                 target-mode))
        (gn-fct2 (intern (format "pel--setup-for-%s" target-mode)))
        (gn-docstring2 (format "Set the environment for %s buffers."
                               target-mode))
        (gn-mode-name (intern (format "%s-mode" target-mode)))
        (gn-ts-mode-name (intern (format "%s-ts-mode" target-mode)))
        (gn-mode-hook (intern (format "%s-mode-hook" target-mode)))
        (gn-ts-mode-hook (intern (format "%s-ts-mode-hook" target-mode)))
        (gn-minor-modes (intern (format "pel-%s-activates-minor-modes"
                                        target-mode)))
        (gn-use      (intern (format "pel-use-%s" target-mode)))
        (gn-use-tabs (intern (format "pel-%s-use-tabs"
                                     target-mode)))
        (gn-tab-width (intern (format "pel-%s-tab-width"
                                      target-mode)))
        (gn-tie-indent-2-tab (intern
                              (format "pel-%s-tie-indent-to-tab-width"
                                      target-mode)))
        (gn-fname       (file-name-base (macroexp-file-name)))
        (newbody nil)
        (hook-body nil))
    ;; Add code to newbody in order: some code is placed *before* BODY
    ;; to allow BODY to see the values and possibly modify them.
    ;; Some code is added *after* the BODY.  BODY is a list.
    ;;
    ;; 1 - Code before BODY
    ;; If the major mode is not one of the modes that do not need
    ;; to support hard-tab control and width create code that set them
    (unless (memq target-mode pel--tab-controlling-major-modes)
      ;; Starting with Emacs 30, org-mode only supports a tab-width of 8
      (unless (and pel-emacs-30-or-later-p
                   (eq target-mode 'org))
        (pel-append-to newbody
          `((pel-setq-local-unless-filevar tab-width ,gn-tab-width))))
      (pel-append-to newbody
        `((pel-setq-local-unless-filevar indent-tabs-mode ,gn-use-tabs)))
      (when (boundp gn-tie-indent-2-tab)
        (pel-append-to newbody
          `((pel--set-indent-control-variables ,gn-tie-indent-2-tab)))))

    ;; - Add tree sitter control if necessary
    ;; — only :same-for-ts triggers the in-hook remap-alist update;
    ;;         :same-for-ts-early-remap suppresses it (caller handles it
    ;;         eagerly)
    (when (and (eq ts-option :same-for-ts)
               (boundp 'major-mode-remap-alist))
      ;; There are no reasons to use major-mode when the major-ts-mode
      ;; mode is available and working.  Therefore, when the tree-sitter mode
      ;; is requested by the user for this major mode, ensure that whenever
      ;; major-mode is requested, major-ts-mode is used.
      ;; See: https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?h=emacs-30#n123
      (pel-append-to newbody
        `((when (pel-treesit-remap-available-for (quote ,target-mode))
            (if (eq ,gn-use 'with-tree-sitter)
                (add-to-list (quote major-mode-remap-alist)
                             (quote
                              (,gn-mode-name . ,gn-ts-mode-name)))
              (add-to-list (quote major-mode-remap-alist)
                           (quote (,gn-mode-name))))))))
    ;;
    ;; 2 - Include BODY
    (pel-append-to newbody body)
    ;;
    ;; 3 - Include code that must be done *after* BODY:
    ;;
    ;; When the <f12> key prefixes are defined, set them up first
    ;; in the function body to ensure they are available and will not shadow
    ;; another call to `pel-local-set-f12-M-f12' that wants to install a
    ;; sub-prefix.
    (when (and key-prefix
               (not (eq key-prefix :no-f12-keys)))
      (pel-append-to newbody
        `((pel-local-set-f12-M-f12 (quote ,key-prefix)))))

    ;; Add the code that activates the minor modes identified by the
    ;;`pel-<mode>-activates-minor-modes' user-option, and other PEL
    ;; user options:
    ;; - `pel-fly-engine-for-modes' & `pel-files-activating-syntax-check'
    (pel-append-to newbody
      `((pel-turn-on-local-minor-modes-in
         (quote ,gn-minor-modes))
        (pel-check-minor-modes-in ,gn-minor-modes)
        (pel--auto-activate-fly)))

    ;; 4 - Prepare the code that is invoked after the newbody
    (pel-append-to hook-body
      `((declare-function ,gn-fct2 ,gn-fname)
        (defun ,gn-fct1 ()
          ,gn-docstring1
          (add-hook 'hack-local-variables-hook
                    (function ,gn-fct2) nil t))
        (declare-function ,gn-fct1 ,gn-fname)))

    ;; 4.1 - Append support for classic mode if necessary
    (unless (eq ts-option :ts-only)
      (pel-append-to hook-body
        `((pel--mode-hook-maybe-call (function ,gn-fct1)
                                     (quote ,gn-mode-name)
                                     (quote ,gn-mode-hook)))))
    ;; 4.1 - Append support for ts-mode if necessary
    (when (memq ts-option '(:ts-only :same-for-ts :independent-ts
                                     :same-for-ts-early-remap))
      (pel-append-to hook-body
        `((pel--mode-hook-maybe-call (function ,gn-fct1)
                                     (quote ,gn-ts-mode-name)
                                     (quote ,gn-ts-mode-hook)))))

    ;; 5 - Return the following generated code:
    `(progn
       (defun ,gn-fct2 ()
         ,gn-docstring2
         (progn
           ,@newbody))
       (progn
         ,@hook-body))))


;; ---------------------------------------------------------------------------

(eval-and-compile
  (defun pel--empty-form-p (form)
    "Return non-nil if FORM is trivially empty at macro-expansion time.
A form is considered empty when it is nil or an empty `progn': (progn)."
    (or (null form)
        (equal form '(progn)))))


(defmacro pel-setup-major-mode (features
                                target-mode key-prefix ts-option
                                init-body after-load-body
                                &rest config-body)
  "Configure a major mode with tree-sitter awareness.

Arguments:
FEATURES        A feature symbol or unquoted list of feature symbols
                (same as the first argument of `pel-eval-after-load').
                Used to time the `pel-config-major-mode' setup.

TARGET-MODE     The bare mode symbol, e.g. \\='python (same meaning as in
                `pel-config-major-mode').

KEY-PREFIX      The <f12>/<M-f12> key prefix (same as
                `pel-config-major-mode').

TS-OPTION       Tree-sitter option, one of :no-ts :ts-only :same-for-ts
                :independent-ts (same meaning as in `pel-config-major-mode').

INIT-BODY       A single form (possibly a `progn') executed immediately at
                initialization time, before Emacs opens any files.
                Typical use: `auto-mode-alist' registrations.

AFTER-LOAD-BODY  A single form executed inside each `with-eval-after-load'
                 block (via `pel-eval-after-load') but *before* the
                `pel-config-major-mode' hook setup.

CONFIG-BODY     Zero or more forms passed as the body to
                `pel-config-major-mode'; they run inside the mode-hook
                `hack-local-variables-hook'.


Tree-sitter handling (the Python fix):
  When TS-OPTION is `:same-for-ts', the `major-mode-remap-alist' entry that
  redirects the classic mode to the tree-sitter variant is established *eagerly*
  at step 1 (init time), before any file is ever opened.  This guarantees the
  very first Python (or other language) buffer is already redirected.
  `pel-config-major-mode' is invoked with `:same-for-ts-early-remap', which
  registers hooks for both the classic and TS modes without duplicating the
  remap-alist update."
  (declare (indent 4))
  (unless (memq ts-option '(:no-ts :ts-only :same-for-ts :independent-ts))
    (error "pel-setup-major-mode: ts-option must be :no-ts, :ts-only, \
:same-for-ts or :independent-ts; got: %S" ts-option))
  (let* (;; When :same-for-ts, tell pel-config-major-mode that the remap-alist
         ;; entry is already taken care of by us; otherwise pass through as-is.
         (internal-ts-option (if (eq ts-option :same-for-ts)
                                 :same-for-ts-early-remap
                               ts-option))
         ;; Runtime symbol names derived from TARGET-MODE
         (gn-use-var    (intern (format "pel-use-%s"    target-mode)))
         (gn-mode-name  (intern (format "%s-mode"       target-mode)))
         (gn-ts-mode    (intern (format "%s-ts-mode"    target-mode))))
    `(progn
       ;; ---------------------------------------------------------------
       ;; Step 1: Run INIT-BODY immediately at startup.
       ;; Typical content: auto-mode-alist entries, package-specific
       ;; setup that must be visible before any file is opened.
       ;; Omitted entirely when init-body is nil or (progn).
       ,@(unless (pel--empty-form-p init-body)
           (list init-body))

       ;; ---------------------------------------------------------------
       ;; Step 2: Eagerly register the major-mode-remap-alist entry when
       ;; Emacs supports it.
       ;; - This must execute at init time: ;; Emacs consults
       ;;   `major-mode-remap-alist' before activating (and thus before any
       ;;   mode hook can run), so a hook-based approach would always miss
       ;;   the first opened buffer.
       ;; - Generated only when ts-option is :same-for-ts.
       ,@(when (eq ts-option :same-for-ts)
           `((when (and (eq ,gn-use-var 'with-tree-sitter)
                        (boundp 'major-mode-remap-alist)
                        (pel-treesit-remap-available-for (quote ,target-mode)))
               (add-to-list (quote major-mode-remap-alist)
                            (quote (,gn-mode-name . ,gn-ts-mode))))))

       ;; ---------------------------------------------------------------
       ;; Step 3: Deferred setup — everything that needs the mode feature
       ;; to already be loaded.
       ;; AFTER-LOAD-BODY runs first, then pel-config-major-mode installs
       ;; the mode-hook machinery (key bindings, minor-mode activation, …).
       ;; pel-config-major-mode is called with :same-for-ts-early-remap
       ;; (instead of :same-for-ts) to register hooks for both classic and
       ;; TS modes without re-inserting the remap-alist entry.
       (pel-eval-after-load ,features
         ;; Omitted entirely when after-load-body is nil or (progn).
         ,@(unless (pel--empty-form-p after-load-body)
             (list after-load-body))
         ;;
         (pel-config-major-mode ,target-mode ,key-prefix ,internal-ts-option
           ,@config-body)))))

;;; --------------------------------------------------------------------------
(provide 'pel--install)

;;; pel--install.el ends here

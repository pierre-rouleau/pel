;;; pel--install.el --- PEL file and package installation and lazy loading.  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 12 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-13 10:18:21 EDT, updated by Pierre Rouleau>

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
;; Speedbar Support
;; - `pel-add-speedbar-extension'
;;
;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)

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
                        (when (string= (buffer-substring-no-properties 1 4)
                                       "404")
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
            (make-directory utils-dirname :make-parents-if-needed))
          (when subdir
            (setq subdir (expand-file-name subdir utils-dirname))
            (unless (file-exists-p subdir)
              (make-directory subdir :make-parents-if-needed)))

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
                                            &optional refresh)
  "Download & install FNAME from Gitlab user and project into PEL utils.
GITLAB-USER is the name of Gitlab user.
GITLAB-PROJECT is the name of Gitlab project.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil.

The function returns t if the file was downloaded, nil otherwise.
Permission errors are raised but install failures are just reported
by warning to prevent init from failing."
  (pel-install-file (format "https://gitlab.com/%s/%s/-/raw/master/%s"
                            gitlab-user
                            gitlab-project
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

WITH-PEL-INSTALL: describe how to install the package:
- nil:       Install PACKAGE with `pel-install-package'.
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

;;; --------------------------------------------------------------------------
(provide 'pel--install)

;;; pel--install.el ends here

# Makefile --- GNU Make to build the PEL distributable package
#
# Copyright (C) 2020 by Pierre Rouleau

# Author: Pierre Rouleau <prouleau001@gmail.com>
# Last Modified Time-stamp: <2020-08-27 17:27:21, updated by Pierre Rouleau>
# Keywords: packaging, build-control

# This file is part of the PEL package
# This file is not part of GNU Emacs.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# -----------------------------------------------------------------------------
# Description:
#
# To get a description on how to use this makefile, execute "make help".
#
# - Tested with macOS GNU Make version 3.81

# -----------------------------------------------------------------------------
# Portable makefile
.POSIX:

# -----------------------------------------------------------------------------
# allow overriding the Emacs binary at the command line
EMACS = emacs

# Note: the above macro allows the following use of make with
# other Emacs binaries:
#
#    make clean
#    make EMACS=emacs-26.1 pel test
#    make clean
#    make EMACS=emacs-24.3 pel test
#

# -----------------------------------------------------------------------------
# Define the location of the normal Emacs initialization file.
# This is required for elisp-lint so that it can find the elisp-lint
# and its dependencies.  This can be changed on the command line.
EMACS_INIT = "~/.emacs.d/init.el"

# -----------------------------------------------------------------------------
# PEL Package Version - increase this number on each release
PEL_VERSION := 0.1.1

# NOTE: Also update the version numbers in the following files:
# - NEWS
# - pel--base.el
# - pel.el
# - pel-pkg.el
# - pel-manual.rst

# -----------------------------------------------------------------------------
# Directory Used in this build

# SRC_DIR   : where all PEL .el file are stored
SRC_DIR := .

# OUT_DIR   : where the pel archive tar file is stored
OUT_DIR := out

# PELPA_DIR : the local Elpa-compliant Emacs Package Archive directory
#             where PEL package tar file is installed and then used
#             to install PEL in user's Emacs.
PELPA_DIR := pelpa

# TMP_DIR   : where the directory holding files to create the PEL package tar
#             are stored
TMP_DIR := tmp-copies

# DEST_DIR  : where the PEL source code files are stored to create the tar file.
#             This directory has a name that includes PEL's version to comply
#             with Emacs management system.
DEST_DIR := $(TMP_DIR)/pel-$(PEL_VERSION)

# DEST_TEST_DIR : the directory where PEL test source code files located for
#                 the creation of PEL tar file, when it is created to include
#                 the PEL test files.
#                 Note that while PEL is distributed via its Git repository,
#                 these files do not need to be included in the PEL tar file.
DEST_TEST_DIR    := $(DEST_DIR)/test

# DEST_DOC_PDF_DIR : the directory where the PDF files that are included in
#                 PEL package tar file when these files are included.
#                 Note that while PEL is distributed via its Git repository,
#                 these files do not need to be included in the PEL tar file.
DEST_DOC_PDF_DIR := $(DEST_DIR)/doc/pdf

# -----------------------------------------------------------------------------
# Identify the files used in the package.

# The Emacs Lisp files that must be byte-compiled to check their validity.
# IMPORTANT:
#    - The first files the pel--base, pel--macros and pel--options,
#    - then the pel- files which use the pel-- files,
#    - The last two must be pel_keys followed by pel.
#    - The file names are selected to impose that order when files
#      are byte compiled by a process that select files alphabetically.
EL_FILES := pel--base.el \
			pel--macros.el \
			pel--options.el \
			pel-applescript.el \
			pel-autocomplete.el \
			pel-autoload.el \
			pel-bookmark.el \
			pel-ccp.el \
			pel-comment.el \
			pel-comment-adorn.el \
			pel-commonlisp.el \
			pel-completion.el \
			pel-cua.el \
			pel-cursor.el \
			pel-emacs.el \
			pel-erlang.el \
			pel-erlang-skels.el \
			pel-face-ut.el \
			pel-file.el \
			pel-fill.el \
			pel-font.el \
			pel-frame-control.el \
			pel-fs.el \
			pel-graphviz-dot.el \
			pel-help.el \
			pel-hideshow.el \
			pel-hide-docstring.el \
			pel-highlight.el \
			pel-imenu.el \
			pel-indent.el \
			pel-key-chord.el \
			pel-kbmacros.el \
			pel-line-control.el \
			pel-lisp.el \
			pel-list.el \
			pel-mark.el \
			pel-navigate.el \
			pel-numkpad.el \
			pel-open.el \
			pel-pathmng.el \
			pel-plantuml.el \
			pel-prompt.el \
			pel-read.el \
			pel-register.el \
			pel-regexp.el \
			pel-rst.el \
			pel-scroll.el \
			pel-search.el \
			pel-search-regexp.el \
			pel-seq.el \
			pel-skels.el \
			pel-skels-c.el \
			pel-skels-elisp.el \
			pel-skels-rst.el \
			pel-speedbar.el \
			pel-spell.el \
			pel-tags.el \
			pel-tempo.el \
			pel-text-insert.el \
			pel-text-transform.el \
			pel-undo.el \
			pel-uuid.el \
			pel-window.el \
			pel-xr.el \
			pel.el

# Files not byte compiled alone but still included in the package tar file
OTHER_EL_FILES := pel_keys.el pel-pkg.el pel-autoloads.el

# Miscellaneous files to take verbatim inside the package tar file
OTHER_FILES := README

# Emacs Regression Test files that uses ert, to test and include in tar file.
TEST_FILES := pel-file-test.el pel-text-transform-test.el

# Documentation PDF files to copy verbatim into the doc/pdfs
PDF_FILES := -legend.pdf \
			abbreviations.pdf \
			align.pdf \
			auto-completion.pdf \
			autosave-backup.pdf \
			bookmarks.pdf \
			buffers.pdf \
			case-conversion.pdf \
			closing-suspending.pdf \
			comments.pdf \
			completion-input.pdf \
			counting.pdf \
			customize.pdf \
			cut-paste.pdf \
			diff-merge.pdf \
			display-lines.pdf \
			drawing.pdf \
			enriched-text.pdf \
			ert.pdf \
			faces-fonts.pdf \
			file-mngt.pdf \
			file-variables.pdf \
			filling-justification.pdf \
			frames.pdf \
			graphviz-dot.pdf \
			grep.pdf \
			help.pdf \
			hide-show-code.pdf \
			highlight.pdf \
			hooks.pdf \
			indentation.pdf \
			input-method.pdf \
			inserting-text.pdf \
			keyboard-macros.pdf \
			key-chords.pdf \
			keys-f11.pdf \
			keys-fn.pdf \
			macOS-terminal-settings.pdf \
			marking.pdf \
			menus.pdf \
			mode-dired.pdf \
			mode-org-mode.pdf \
			mode-rst.pdf \
			modifier-keys.pdf \
			mouse.pdf \
			narrowing.pdf \
			navigation.pdf \
			numkeypad.pdf \
			packages.pdf \
			pl-applescript.pdf \
			pl-c++.pdf \
			pl-c.pdf \
			pl-common-lisp.pdf \
			pl-d.pdf \
			pl-emacs-lisp.pdf \
			pl-erlang.pdf \
			rectangles.pdf \
			registers.pdf \
			scrolling.pdf \
			search-replace.pdf \
			sessions.pdf \
			shells.pdf \
			sorting.pdf \
			speedbar.pdf \
			spell-checking.pdf \
			text-modes.pdf \
			tags.pdf \
			transpose.pdf \
			undo-redo-repeat.pdf \
			vsc-mercurial.pdf \
			web.pdf \
			whitespaces.pdf \
			windows.pdf


# SRC_FILES include *all* Emacs Lisp source files that are part of PEL,
#           as well as the miscellaneous files that must be distributed
#           inside the PEL package tar file.
#           This excludes the test files, the Emacs Lisp files used to
#           build and install PEL (used by this Makefile) and the PDF
#           document files.
SRC_FILES := $(OTHER_EL_FILES) $(EL_FILES) $(OTHER_FILES)

# TARGET_SOURCE_FILES lists all Emacs Lisp source files that are part
#           of PEL as well as the miscellaneous files that must be
#           distributed inside the PEL package tar file.
#           The list of files are set to have the path where to store
#           the files to create the tar file.
TARGET_SOURCE_FILES := $(patsubst %,$(DEST_DIR)/%,$(SRC_FILES))

# TARGET_PDF_FILES lists the PDF doc files with a path identifying
#           where they should be stored when those files are included
#           in the PEL tar file.
#           Note: at the moment this is not used.
# TARGET_PDF_FILES := $(patsubst %,$(DEST_DOC_PDF_DIR)/%,$(PDF_FILES))

# TARGET_TEST_FILES lists the test files with a path identifying
#           where they should be stored when those files are included
#           in the PEL tar file.
#           Note: at the moment this is not used.
# TARGET_TEST_FILES := $(patsubst %,$(DEST_TEST_DIR)/%,$(TEST_FILES))

# ELC_FILES list the PEL .elc files
ELC_FILES := $(subst .el,.elc,$(EL_FILES))

# PEL_TAR_FILE makes the name of the PEL tar file name (with PEL version)
PEL_TAR_FILE := pel-$(PEL_VERSION).tar

# -----------------------------------------------------------------------------
# First rule, allows 'make' command to build everything that needs updating

# 1: First build the .elc files to check for errors.
# 2: Then run the integration tests.
# 3: If all is OK, create the PEL Emacs package tar file
# 4: Install that tar file into the local Elpa-compliant directory,
#    ready to be used by Emacs.

all: pel test pkg mypelpa

# -----------------------------------------------------------------------------
# Self-desciptive rule: make help prints the info.

.PHONY: help
help:
	@printf "\nBuild the Emacs PEL package file for distribution.\n"
	@printf "\n"
	@printf "Currently building PEL version $(PEL_VERSION).\n"
	@printf "1) First byte-compile all Emacs Lisp files in required order.\n"
	@printf "2) Then runs the regression tests\n"
	@printf "3) Encapsulate all files for distribution into a compressed tar\n"
	@printf "   file that is copied into the local Emacs package archive, $(PEL_TAR_FILE)\n"
	@printf "   located in the $(OUT_DIR) directory.\n"
	@printf "4) Copy the PEL package tar file into a local package archive for testing.\n"
	@printf "\n"
	@printf "NOTE: All commands must be issued inside the directory where this Makefile\n"
	@printf "      is located.\n"
	@printf "\n"
	@printf "Usage:\n"
	@printf " * make           - builds everything as needed.\n"
	@printf " * make all       - builds everything as needed.\n"
	@printf " * make pel       - byte compile all files. Nothing else done.\n"
	@printf " * make compile   - byte compile all files. Nothing else done.\n"
	@printf " * make lint      - check .el files with several tools via elisp-lint.\n"
	@printf " * make all-dirs  - create all output and temporary directories.\n"
	@printf " * make clean     - remove $(PELPA_DIR) and all output files including $(PEL_TAR_FILE)\n"
	@printf " * make clean_tar - remove the $(OUT_DIR)/$(PEL_TAR_FILE)\n"
	@printf " * make clean_mypelpa - remove the directory $(PELPA_DIR)\n"
	@printf " * make test      - Run the regression tests.\n"
	@printf " * make pkg       - Build the tar file inside the $(OUT_DIR) directory.\n"
	@printf " * make mypelpa   - Copy the tar file into a local package archive.\n"
	@printf "\n"
	@printf "LIMITATIONS:\n"
	@printf "  - The package version number must be updated inside several\n"
	@printf "    files:\n"
	@printf "    - Makefile\n"
	@printf "    - pel.el\n"
	@printf "    - pel-pkg.el\n"
	@printf "\n"

# -----------------------------------------------------------------------------
# Make Script checking (debugging) facilities
# Use them to see the expanded values

.PHONY: check
check: 	check-version \
		check-src-dir \
		check-dest-dir \
		check-dest-test-dir \
		check-doc-pdf \
		check-target \
		check-elc-files

check-version:
	@echo PEL_VERSION = $(PEL_VERSION)

check-src-dir:
	@echo SRC_DIR = $(SRC_DIR)

check-dest-dir:
	@echo DEST_DIR = $(DEST_DIR)

check-dest-test-dir:
	@echo DEST_TEST_DIR = $(DEST_TEST_DIR)

check-doc-pdf:
	@echo DEST_DOC_PDF_DIR = $(DEST_DOC_PDF_DIR)

check-target:
	@echo TARGET_SOURCE_FILES = "( $(TARGET_SOURCE_FILES) )"

check-elc-files:
	@echo ELC_FILES = "( $(ELC_FILES) )"

# -----------------------------------------------------------------------------
# Creating the target directories when they don't exist.

all-dirs:	$(OUT_DIR) \
			$(PELPA-DIR) \
			$(TMP_DIR) \
			$(DEST_DIR) \
			$(DEST_TEST_DIR) \
			$(DEST_DOC_PDF_DIR)

$(OUT_DIR):
	mkdir -p $@

$(PELPA_DIR):
	mkdir -p $@
	@echo "(1)" > $@/archive-contents

$(TMP_DIR):
	mkdir -p $@

$(DEST_DIR):
	mkdir -p $@

$(DEST_TEST_DIR):
	mkdir -p $@

$(DEST_DOC_PDF_DIR):
	mkdir -p $@

# -----------------------------------------------------------------------------
# Rules to copy files to DEST_DIR directory to build the Emacs package tar file

$(DEST_DIR)/%.el: $(SRC_DIR)/%.el
				cp $< $@

$(DEST_DIR)/README: $(SRC_DIR)/README
				cp $< $@

# While PEL is distributed through its Git repo, there's no need to store the
# test files and the PDF documentation files inside PEL's package tar file.
# If PEL gets distributed through MELPA, then it's possible that we'd like
# these files to be stored inside the tar file.  If so, then un-comment the
# following lines.
#
# $(DEST_TEST_DIR)/%.el:     $(SRC_DIR)/test/%.el
# 				cp $< $@
#
# $(DEST_DOC_PDF_DIR)/%.pdf: $(SRC_DIR)/doc/pdf/%.pdf
# 				cp $< $@

# -----------------------------------------------------------------------------
# Emacs Lisp file dependencies
# ----------------------------
#
# The dependencies are not required to identify what file to byte-compile
# from scratch: the file in order is sufficient.  The dependencies are required
# to identify minimal byte-compilations when files are modified after their first
# byte-compilation but also on the very first compilation: it alters the order of
# byte-compilation for the pel- files.


pel-applescript.elc:    pel-read.elc
pel-autocomplete.elc:   pel--base.elc pel--macros.elc pel--options.elc
pel-autoload.elc:       pel--options.elc
pel-bookmark.elc:       pel--base.elc
pel-ccp.elc:            pel-navigate.elc
pel-comment.elc:        pel--base.elc
pel-comment-adorn.elc:  pel-rst.elc pel-comment.elc pel-mark.elc
pel-completion.elc:     pel--base.elc pel--macros.elc pel--options.elc pel-seq.elc pel-prompt.elc
pel-cursor.elc:         pel--options.elc
pel-erlang.elc:         pel--options.elc pel-fs.elc
pel-erlang-skels.elc:   pel--base.elc pel--options.elc pel--macros.elc pel-list.elc pel-tempo.elc pel-skels.elc
pel-file.elc:           pel--base.elc pel-prompt.elc pel-read.elc pel-window.elc
pel-fill.elc:           pel--base.elc
pel-frame-control.elc:  pel--base.elc
pel-graphviz-dot.elc:   pel--base.elc pel-ccp.elc
pel-hide-docstring.elc: pel-face-ut.elc pel-navigate.elc
pel-highlight.elc:      pel--base.elc
pel-imenu.elc:          pel--base.elc
pel-indent.elc:         pel-ccp.elc pel-mark.elc
pel-kbmacros.elc:       pel--options.elc
pel-key-chord.elc:      pel--base.elc pel--options.elc
pel-lisp.elc:           pel--base.elc
pel-mark.elc:           pel--base.elc
pel-navigate.elc:       pel--base.elc pel-scroll.elc
pel-numkpad.elc:        pel--base.elc pel-ccp.elc pel-navigate.elc pel-scroll.elc
pel-open.elc:           pel-file.elc pel-rst.elc
pel-pathmng.elc:        pel-window.elc
pel-plantuml.elc:       pel--base.elc
pel-prompt.elc:         pel--base.elc
pel-read.elc:           pel-navigate.elc
pel-register.elc:       pel--base.elc
pel-regexp.elc:         pel--base.elc
pel-rst.elc:            pel--base.elc pel--macros.elc pel--options.elc pel-bookmark.elc pel-file.elc
pel-search:             pel--options.elc pel--macros.elc pel-prompt.elc pel-read.elc pel-window.elc
pel-skels.elc:          pel--base.elc pel-prompt.elc
pel-skels-c.elc:        pel--base.elc pel--options.elc pel--macros.elc pel-prompt.elc pel-skels.elc pel-tempo.elc pel-text-insert.elc pel-uuid.elc
pel-skels-elisp.elc:    pel--base.elc pel--options.elc pel-prompt.elc pel-skels.elc pel-tempo.elc pel-text-insert.elc
pel-skels-rst.elc:      pel-prompt.elc pel-skels.elc pel-tempo.elc pel-text-insert.elc
pel-speedbar.elc:       pel--base.elc pel--macros.elc pel--options.elc
pel-spell.elc:          pel--macros.elc pel--base.elc
pel-tags.elc:           pel--base.elc
pel-text-insert.elc:    pel--base.elc pel--macros.elc pel-window.elc
pel-text-transform.elc: pel--base.elc
pel-window.elc:         pel--base.elc
pel-xr.elc:             pel--base.elc pel-read.elc
pel_keys.elc:           pel--base.elc pel--macros.elc pel--options.elc pel-completion.elc pel-search.elc

# -----------------------------------------------------------------------------
# Rules to byte-compile the Emacs-Lisp source code files

# Byte-compile all PEL files in a bare-bones Emacs (emacs -Q), one file at
# a time.  Byte-compile all files except pel_keys.el, which is the key
# bindings with use-package forms.
# Compiling pel_keys.el would cause installation of external packages.

# Single .el file byte-compile to .elc rule
.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q  --batch -L . -f batch-byte-compile $<


# Target to byte-compile all Emacs Lisp files inside one Emacs Session.
# Compile all without any init configuration.
# Compile pel_keys.el last, *with* init.el so it can find the external packages.
# Note that pel_keys.el is a *canned* init.el that is byte-compiled to increase
# its speed as much as possible and also checking as much as possible.
compile: pel

pel: $(ELC_FILES)
	$(EMACS) -Q --batch -L . -l $(EMACS_INIT) -f batch-byte-compile pel_keys.el


# Target to control file linting with the elisp-lint package.
# This requires access to a load-path that can find elisp-lint as well
# as all the tools it uses and all packages used by PEL.
# This is why the Emacs init file is loaded.
.PHONY: lint
lint:
	$(EMACS) -Q --batch -L . -l $(EMACS_INIT) -l elisp-lint.el -f elisp-lint-files-batch \
			 --no-package-format $(EL_FILES) pel_keys.el
	$(EMACS) -Q --batch -L . -l $(EMACS_INIT) -l elisp-lint.el -f elisp-lint-files-batch \
			 pel.el

# -----------------------------------------------------------------------------
# Integration test rules
#
# PEL uses the ERT package to run tests.

.PHONY: test
test:
	@printf "***** Running Integration tests\n"
	$(EMACS) --batch -L . -l ert -l test/pel-file-test.el -f ert-run-tests-batch-and-exit

# -----------------------------------------------------------------------------
# Dependency rule to create the directory used for creating a Tar file and
# copy files into proper locations inside that directory tree.

.PHONY: a-copy
a-copy: $(OUT_DIR) \
		$(DEST_DIR) \
		$(TARGET_SOURCE_FILES)


#		$(DEST_TEST_DIR) \
#		$(TARGET_TEST_FILES) \
#		$(DEST_DOC_PDF_DIR)  \
#		$(TARGET_PDF_FILES)

# -----------------------------------------------------------------------------
# Distribution tar package file creation rule

# Create the out directory if it does not exist.
# The pipe tests that the out pre-requisite is order only,
# not depending on time stamp of directory or its files.
# * Implementation Note:
#   * on macOS: tar may include several files that have names that start with "./._".
#
#    According to https://unix.stackexchange.com/questions/282055/a-lot-of-files-inside-a-tar
#    and https://unix.stackexchange.com/questions/9665/create-tar-archive-of-a-directory-except-for-hidden-files
#    there is two ways to prevent inclusion of these hidden files in macOS:
#    - 1) use the --disable-copyfile tar option, available in later versions of macOS tar
#    - 2) set the environment variable COPYFILE_DISABLE=1 for when tar is run (via a target export)
#    Apparently it's also possible to build the tar file while no Finder is opened for the
#    directories that contain the files that tar includes.  I find this last method dangerous
#    since I don't know how to easily detect if Finder is opened on the particular directories.
#    So I tested method 1 and 2 while Finder was opened on the directory.  And it worked.
#    I selected option 2, since it is compatible with older versions of macOS tar and is also
#    likely not going to affect tar running on other OS.

.PHONY: pkg
pkg: 	export COPYFILE_DISABLE=1


# Note: Removing the TMP_DIR tree after creating the tarball file,
#       it's no longer needed.  If we leave it there Emacs will include
#       these files in a ELisp cross reference search.
pkg: | a-copy
	@printf "***** Create the PEL package TAR file\n"
	rm -f $(OUT_DIR)/$(PEL_TAR_FILE)
	tar -C $(TMP_DIR) -cvf $(OUT_DIR)/$(PEL_TAR_FILE) pel-$(PEL_VERSION)/
	rm -rf $(TMP_DIR)
	ls -l $(OUT_DIR)/$(PEL_TAR_FILE)

# -----------------------------------------------------------------------------
# Installation of package Tar inside the local package archive

# The command will only succeed if the version of the new PEL package has a
# version newer than the version already inside the archive.
# If you want to replace the package with the same version you have to edit
# archive-contents file and remove the entry for PEL inside it.

mypelpa: $(PELPA_DIR)
	$(EMACS) --batch -L . -l $(EMACS_INIT) -l install-pel.el -f upload-pel-to-local-archive

# -----------------------------------------------------------------------------
# Cleanup rules

# Remove the tar file from the local Emacs archive, without complaining
# if it is not present.
# The -f option prevents complaints from rm when the file is not present.

.PHONY: clean-tar

clean-tar:
	-rm -f $(OUT_DIR)/$(PEL_TAR_FILE)

clean-mypelpa:
	-rm -rf $(PELPA_DIR)

clean: clean-tar clean-mypelpa
	-rm *.elc
	-rm -rf $(OUT_DIR)
	-rm -rf $(TMP_DIR)

# -----------------------------------------------------------------------------

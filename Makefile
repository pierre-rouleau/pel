# Makefile --- GNU Make to build the PEL distributable package
#
# Copyright (C) 2020 by Pierre Rouleau

# Author: Pierre Rouleau <prouleau.swd@gmail.com>
# Last Modified Time-stamp: <2020-03-20 12:16:32, updated by Pierre Rouleau>
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
# To get a description on how to use this makefile, execute "male help".
#
# - Tested with macOS GNU Make version 3.81

# -----------------------------------------------------------------------------
# PEL Package Version - increase this number on each release
PEL_VERSION := 0.0.1

# -----------------------------------------------------------------------------
# Define the directories involved

# sub-directories where make stores new files
SRC_DIR          := .
OUT_DIR          := out
TMP_DIR          := tmp-copies
DEST_DIR         := $(TMP_DIR)/pel-$(PEL_VERSION)
DEST_TEST_DIR    := $(DEST_DIR)/test
DEST_DOC_PDF_DIR := $(DEST_DIR)/doc/pdf

# Directory where to store the built package.
OUT_REPO_DIR := ~/dev/emacs-archive

# -----------------------------------------------------------------------------
# Identify the files used in the package.

# The Emacs Lisp files that must be byte-compiled to check their validity.
# IMPORTANT:
#    - The first two must be pel--base, followed by pel--options.
#    - The last two must be pel-zkeys followed by pel.
EL_FILES := pel--base.el \
			pel--options.el \
			pel-autocomplete.el \
			pel-autoload.el \
			pel-bookmark.el \
			pel-ccp.el \
			pel-comment.el \
			pel-commonlisp.el \
			pel-cua.el \
			pel-file.el \
			pel-fill.el \
			pel-font.el \
			pel-frame-control.el \
			pel-highlight.el \
			pel-imenu.el \
			pel-indent.el \
			pel-kbmacros.el \
			pel-line-control.el \
			pel-lisp.el \
			pel-mark.el \
			pel-navigate.el \
			pel-numkpad.el \
			pel-prompt.el \
			pel-register.el \
			pel-rst.el \
			pel-scroll.el \
			pel-search.el \
			pel-speedbar.el \
			pel-spell.el \
			pel-text-insert.el \
			pel-text-transform.el \
			pel-window.el \
			pel-zkeys.el \
			pel.el

# Files not byte compiled but still included in the package tar file
OTHER_EL_FILES := pel-pkg.el pel-autoloads.el

# Miscellaneous files to take verbatim inisde the tar file
OTHER_FILES := README

# Emacs Regression Test files that uses ert, to test and include in tar file.
TEST_FILES := pel-file-test.el

# Documentation PDF files to copy verbatim into the doc/pdfs
PDF_FILES := -legend.pdf \
			abbreviations.pdf \
			align.pdf \
			bookmarks.pdf \
			buffers.pdf \
			case-conversion.pdf \
			closing-suspending.pdf \
			comments.pdf \
			counting.pdf \
			cut-paste.pdf \
			display-lines.pdf \
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
			highlight.pdf \
			hooks.pdf \
			indentation.pdf \
			input-method.pdf \
			inserting-text.pdf \
			keyboard-macros.pdf \
			keys-fn.pdf \
			marking.pdf \
			menus.pdf \
			mode-dired.pdf \
			mode-org-mode.pdf \
			mode-rst.pdf \
			modifier-keys.pdf \
			narrowing.pdf \
			navigation.pdf \
			packages.pdf \
			pl-common-lisp.pdf \
			pl-emacs-lisp.pdf \
			registers.pdf \
			scrolling.pdf \
			search-replace.pdf \
			shells.pdf \
			sorting.pdf \
			speedbar.pdf \
			spell-checking.pdf \
			text-modes.pdf \
			transpose.pdf \
			undo-redo-repeat.pdf \
			vsc-mercurial.pdf \
			web.pdf \
			whitespaces.pdf \
			windows.pdf


SRC_FILES := $(OTHER_EL_FILES) $(EL_FILES) $(OTHER_FILES)

# $(TEST_FILES)

TARGET_SOURCE_FILES := $(patsubst %,$(DEST_DIR)/%,$(SRC_FILES))

TARGET_PDF_FILES := $(patsubst %,$(DEST_DOC_PDF_DIR)/%,$(PDF_FILES))

TARGET_TEST_FILES := $(patsubst %,$(DEST_TEST_DIR)/%,$(TEST_FILES))

ELC_FILES := $(subst .el,.elc,$(EL_FILES))

PEL_TAR_FILE := pel-$(PEL_VERSION).tar

# -----------------------------------------------------------------------------
# First rule, allows 'make' command to build everything that needs updating

# first build the .elc files to check for errors
# then run the integration tests
# if all is OK, complete by packaging the files into a Emacs package tar file.

all: pel test pkg myelpa

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
	@printf " * make clean     - remove all output files including $(PEL_TAR_FILE)\n"
	@printf " * make clean_tar - remove the $(OUT_DIR)/$(PEL_TAR_FILE)\n"
	@printf " * make test      - Run the regressin tests.\n"
	@printf " * make pkg       - Build the tar file inside the $(OUT_DIR) directory.\n"
	@printf " * make myelpa    - Copy the tar file into a local package archive.\n"
	@printf "\n"
	@printf "BUGS - Byte-compilation is always done, regardless of the\n"
	@printf "       state of the .el and .elc files.\n"
	@printf "     - The package version number must be updated inside several\n"
	@printf "       files:\n"
	@printf "       - Makefile\n"
	@printf "       - pel.el\n"
	@printf "       - pel-pkg.el\n"
	@printf "\n"

# -----------------------------------------------------------------------------
# Make Script checking (debugging) facilities
# Use them to see the expanded values

.PHONY: check

check: check-version check-src-dir check-dest-dir check-dest-test-dir check-doc-pdf check-target check-elc-files

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

all-dirs:	$(OUT_DIR) $(DEST_DIR) $(OUT_REPO_DIR) $(DEST_DOC_PDF_DIR) $(DEST_TEST_DIR)

$(OUT_DIR):
	mkdir -p $@

$(TMP_DIR):
	mkdir -p $@

$(DEST_DIR):
	mkdir -p $@

$(DEST_TEST_DIR):
	mkdir -p $@

$(DEST_DOC_PDF_DIR):
	mkdir -p $@

$(OUT_REPO_DIR):
	mkdir -p $@

# -----------------------------------------------------------------------------
# Rules to copy files to DEST_DIR directory to build the Emacs package tar file

$(DEST_DIR)/%.el: $(SRC_DIR)/%.el
				cp $< $@

$(DEST_DIR)/README: $(SRC_DIR)/README
				cp $< $@

$(DEST_TEST_DIR)/%.el:     $(SRC_DIR)/test/%.el
				cp $< $@

$(DEST_DOC_PDF_DIR)/%.pdf: $(SRC_DIR)/doc/pdf/%.pdf
				cp $< $@

# -----------------------------------------------------------------------------
# Rules to byte-compile the Emacs-Lisp source code files

# The .el files that are part of the PEL package are all byte-compiled
# together by the build-pel elisp command which knows the dependencies between
# files and which one must be built before the others.  They are all always
# built since there's a small number and they are only built to check for
# errors, and because the process is also relatively fast.
#
# The rule for byte-compiling only one .el file is included since it can help
# in some situations.

.SUFFIXES: .el .elc

# From .el to .elc
.el.elc:
	emacs -batch -L . -l ~/.emacs.d/init.el -f batch-byte-compile $<

# For the moment always perform the build.  It's quick anyway.
pel: $(ELC_FILES)
	@printf "***** Byte Compiling all PEL files in specified order.\n"
	emacs -batch -L . -l ~/.emacs.d/init.el -l build-pel.el -f build-pel

# -----------------------------------------------------------------------------
# Integration test rules
#
# PEL uses the ERT package to run tests.

.PHONY: test

test:
	@printf "***** Running Integration tests\n"
	emacs -batch -L . -l ~/.emacs.d/init.el -l ert -l test/pel-file-test.el -f ert-run-tests-batch-and-exit

# -----------------------------------------------------------------------------
# Dependency rule to create the directory used for creating a Tar file and
# copy files into proper locations inside that directory tree.

.PHONY: a-copy

a-copy: $(OUT_DIR) \
		$(DEST_DIR) \
		$(TARGET_SOURCE_FILES) \
		$(DEST_TEST_DIR) \
		$(TARGET_TEST_FILES) \
		$(DEST_DOC_PDF_DIR)  \
		$(TARGET_PDF_FILES)

# -----------------------------------------------------------------------------
# Distribution tar package file creation rule

# Create the outpkg directory if it does not exist
# The pipe tests that the outpkg pre-requisite is order only,
# not depending on time stamp of directory or its files.
# * Implementation Note:
#   * on macOS: tar may include several files that have names that start with "./._".
#
#      According to https://unix.stackexchange.com/questions/282055/a-lot-of-files-inside-a-tar
#      and https://unix.stackexchange.com/questions/9665/create-tar-archive-of-a-directory-except-for-hidden-files
#      there is two ways to prevent inclusion of these hidden files in macOS:
#      - 1) use the --disable-copyfile tar option, available in later versions of macOS tar
#      - 2) set the environment variable COPYFILE_DISABLE=1 for when tar is run (via a target export)
#      Apparently it's also possible to build the tar file while no Finder is opened for the
#      directories that contain the files that tar includes.  I find this last method dangerous
#      since I don't know how to easily detect if Finder is opened on the particular directories.
#      So I tested method 1 and 2 while Finder was opened on the directory.  And it worked.
#      I selected opton 2, since it is compatible with older versions of macOS tar and is also
#      likely not going to affect tar running on other OS.

.PHONY: pkg

pkg: 	export COPYFILE_DISABLE=1

pkg: | a-copy
	@printf "***** Create the PEL package TAR file\n"
	rm -f $(OUT_DIR)/$(PEL_TAR_FILE)
	tar -C $(TMP_DIR) -cvf $(OUT_DIR)/$(PEL_TAR_FILE) pel-$(PEL_VERSION)/
	ls -l $(OUT_DIR)/$(PEL_TAR_FILE)

# -----------------------------------------------------------------------------
# Installation of package Tar inside the local package archive

# The command will only succeed if the version of the new PEL package has a
# version newer than the version already inside the archive.
# If you want to replace the package with the same version you have to edit
# archive-contents file and remove the entry for PEL inside it.

.PHONY: myelpa

myelpa:
	emacs -batch -L . -l ~/.emacs.d/init.el -l build-pel.el -f upload-pel-to-local-archive

# -----------------------------------------------------------------------------
# Cleanup rules

# Remove the tar file from the local Emacs archive, without complaining
# if it is not present.
# The -f option prevents complaints from rm when the file is not present.

.PHONY: clean-tar

clean-tar:
	rm -f $(OUT_DIR)/$(PEL_TAR_FILE)

clean: clean-tar
	-rm *.elc
	-rm -r $(OUT_DIR)
	-rm -r $(TMP_DIR)

# -----------------------------------------------------------------------------

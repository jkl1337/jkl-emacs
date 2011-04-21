# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#
# To install org-mode, edit the Makefile, type `make', then `make install'.
# To create the PDF and HTML documentation files, type `make doc'.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS=emacs

# Using emacs in batch mode.

BATCH=$(EMACS) -batch -q -no-site-file -eval                             			\
  "(setq load-path (cons (expand-file-name \"./contrib/\") (cons (expand-file-name \"./jkl-lib/\") load-path)))"

# Specify the byte-compiler for compiling org-mode files
ELC= $(BATCH) -f batch-byte-compile

MKDIR = mkdir -p

# How to copy the lisp files and elc files to their distination.
CP = cp -p

LISPF      =  $(wildcard contrib/*.el)
LISPFILES   = $(LISPF)
ELCFILES    = $(LISPFILES:.el=.elc)

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

default: $(ELCFILES)

all:	$(ELCFILES)

.el.elc:
	$(ELC) $<

# Dependencies


###############################################################################
#	@(#)Makefile.inc	1.11 89/09/13 Copyr 1988 SMI
#
#	Copyright (c) 1988 by Sun Microsystems, Inc.
#
# Description:
#	This is the include for all the makefiles. The goals for designing
#	the makefiles follow:
#		1) Each product should know what objects it is made of.
#		   (prevent hidden dependencies)
#		2) The dependency of an object should only be known at
#		   the level of its source file.
#
# Comments:
#	1) The SRC_DIR macro should have been set before including this
#	   file. It is the directory that is the root of the source tree.
#	2) *** Release Engineering - set DESTDIR to what you want.
#	3) Define local cpp flags LOCAL_CPPFLAGS before including this file
#	4) No targets should be defined in this file!
#
###############################################################################

#
# Set the target archecture macro
#
ARCH = $(TARGET_ARCH:-%=%)

#
# Default Release level
#
RELEASE=NOT_SET

#
# Directory to install the programs
#
DESTDIR = $(SRC_DIR)/../release/$(RELEASE)
DESTDIR_SRC = $(DESTDIR)/src
DESTDIR_BIN = $(DESTDIR)/$(ARCH)/bin
DESTDIR_LIB = $(DESTDIR)/$(ARCH)/lib
DESTDIR_INC = $(DESTDIR)/$(ARCH)/include
DESTDIR_DEMO = $(DESTDIR)/$(ARCH)/netlic_sample
DESTDIR_ISAM = $(DESTDIR)/sunisam
DESTDIR_MAN = $(DESTDIR)/$(ARCH)/man

#
# These ENLD products will dissapear when we FCS NLD.
#
DESTDIR_ENLD_BIN = $(DESTDIR)/$(ARCH)/ENLD_bin

#
# Location of the SUN-ISAM library and its name
#
SUNISAM_DIR	= $(SRC_DIR)/../sunisam/$(ARCH)
SUNISAM_LIB_DIR	= $(SUNISAM_DIR)/lib

#
# Uses the alpha isam library
#	Remember to link the right $inc/isam.h file if you change these
#	two lines. isam_old.h is used with libfst_isam.a and isam_new.h
#	is used with libisam.a
#
SUNISAM_LIB_FILE= $(SUNISAM_LIB_DIR)/libisam.a
SUNISAM_LIB	= -lisam
#SUNISAM_LIB_FILE= $(SUNISAM_LIB_DIR)/libfst_isam.a
#SUNISAM_LIB	= -lfst_isam

#
# Location of commonly referenced directories
#
SCHEMA_DIR	= $(SRC_DIR)/schema
PROTO_DIR	= $(SRC_DIR)/protocol
UTILS_DIR	= $(SRC_DIR)/utils
INC_DIR		= $(SRC_DIR)/include
CLNT_LIB	= $(SRC_DIR)/clnt_libs

#
# Command macros and flag definitions
#
AWK	=	awk
CPP	=	/lib/cpp
CPPFLAGS= -I. -I$(INC_DIR) -D$(ARCH) $(LOCAL_CPPFLAGS) 
LPR	=	lpr
#
# Using System V lint
#
#LINT=/usr/5bin/lint
#LINTFLAGS= -c -Dlint
#LINT.c	= $(LINT) $(CPPFLAGS) $(LINTFLAGS)

#
# Flags for Sun OS lint
#
LINTFLAGS= -achx -Dlint

CFLOW=cflow
CFLOWFLAGS = $(CPPFLAGS)
CFLOW_OUT=flow.out

SCCS_CHECK_FILE=$(SRC_DIR)/sccs.outs
SCCS_VERS_BASE=$(DESTDIR)/Module_SIDS
SCCS_VERS_FILE=$(SCCS_VERS_BASE).${ARCH}
SCCS_VERS_ARGS="-d Module:\t:M:\t:I:\t:D:"

#
# Generic targets
#	We leave out release, install and sccs_check because we make
#	the locals for those before the nested
#
TARGETS = all cflow clean debug depend lint profile sccs_check install \
	release tags
TARGETS.nested = $(TARGETS:%=%.nested)
TARGETS.local = $(TARGETS:%=%.local)

#
# C flags for the targets
#
all := CFLAGS= -O
debug := CFLAGS= -g -DDEBUGGING
profile := CFLAGS= -pg -O -DPROFILE
install := CFLAGS= -O
all.local := CFLAGS= -O
debug.local := CFLAGS= -g -DDEBUGGING
profile.local := CFLAGS= -pg -O -DPROFILE
install.local := CFLAGS= -O

#
# Loader flags for the targets
#
all := LDFLAGS= -s $(LOCAL_LDFLAGS)
debug := LDFLAGS= $(LOCAL_LDFLAGS)
profile := LDFLAGS= $(LOCAL_LDFLAGS)
install := LDFLAGS= -s $(LOCAL_LDFLAGS)
release := LDFLAGS= -s $(LOCAL_LDFLAGS)
all.local := LDFLAGS= -s $(LOCAL_LDFLAGS)
debug.local := LDFLAGS= $(LOCAL_LDFLAGS)
profile.local := LDFLAGS= $(LOCAL_LDFLAGS)
install.local := LDFLAGS= -s $(LOCAL_LDFLAGS)
release.local := LDFLAGS= -s $(LOCAL_LDFLAGS)

#
# Lint Flags (Uncomment these if using SYS V lint)
#
#lint := CPP=
#lint.local := CPP=
#lint := CFLAGS=
#lint.local := CFLAGS=

#
# Recursive Targets
#
all := TARGET = all
cflow := TARGET = cflow
clean := TARGET = clean
debug := TARGET = debug
depend := TARGET = depend
install := TARGET = install
lint := TARGET = lint
profile := TARGET = profile
sccs_check := TARGET = sccs_check
release := TARGET = release
tags := TARGET = tags

#
# All makefiles keep the command state
#
.KEEP_STATE:

#    Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# makefile for Ghostscript, Unix/ANSI C/X11 configuration.

# ------------------------------- Options ------------------------------- #

####### The following are the only parts of the file you should need to edit.

# ------ Generic options ------ #

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with a :.
# `pwd` means use the directory in which the 'make' is being done.

GS_LIB_DEFAULT=`pwd`:`pwd`/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# -DDEBUG
#	includes debugging features (-Z switch) in the code.
#	  Code runs substantially slower even if no debugging switches
#	  are set.
# -DNOPRIVATE
#	makes private (static) procedures and variables public,
#	  so they are visible to the debugger and profiler.
#	  No execution time or space penalty.

GENOPT=

# ------ Platform-specific options ------ #

# Define the name of the C compiler.  If the standard compiler for your
# platform is ANSI-compatible, leave this line commented out; if not,
# uncomment the line and insert the proper definition.

#CC=some_C_compiler

# Define the other compilation flags.
# Add -DBSD4_2 for 4.2bsd systems.
# Add -DSYSV for System V or DG/UX.
# Add -DSVR4 (not -DSYSV) for System V release 4.
# The HP 400 seems to want -Aa -w -D_HPUX_SOURCE.
# XCFLAGS can be set from the command line.

CFLAGS=-O $(XCFLAGS)

# Define platform flags for ld.
# SunOS and some others want -X; Ultrix wants -x.
# SunOS 4.n may need -Bstatic.
# XLDFLAGS can be set from the command line.

LDFLAGS=$(XLDFLAGS)

# Define any extra libraries to link into the executable.
# (Libraries required by individual drivers are handled automatically.)

EXTRALIBS=

# Define the include switch(es) for the X11 header files.
# This can be null if handled in some other way (e.g., the files are
# in /usr/include, or the directory is supplied by an environment variable).

XINCLUDE=-I/usr/local/X/include

# Define the directory/ies for the X11 library files.
# This can be null if these files are in the default linker search path.

XLIBDIRS=-L/usr/local/X/lib

# Define the installation commands and target directories for
# executables and files.  Only relevant to `make install'.

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL) -m 775
INSTALL_DATA = $(INSTALL) -m 664

prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib/ghostscript

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.

FEATURE_DEVS=filter.dev dps.dev level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=x11.dev

# ---------------------------- End of options --------------------------- #

# Define the name of the makefile -- used in dependencies.

MAKEFILE=unix-ansi.mak

# Define the ANSI-to-K&R dependency (none for ANSI compilers).

AK=

# Define the compilation rules and flags.

CCC=$(CC) $(CCFLAGS) -c

# --------------------------- Generic makefile ---------------------------- #

# The remainder of the makefile (unixhead.mak, gs.mak, devs.mak, unixtail.mak)
# is generic.  tar_cat concatenates all these together.

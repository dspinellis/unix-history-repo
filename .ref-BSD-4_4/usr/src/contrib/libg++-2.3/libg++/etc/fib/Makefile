# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = m68k-hp-bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
host_canonical = m68k-hp-bsd
target_alias = m68k-hp-bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
target_canonical = m68k-hp-bsd
host_makefile_frag = ../../m68k-hp-bsd.mh
ALL=all.internal
# Makefile for libg++.a

# Copyright (C) 1988, 1992 Free Software Foundation
#   written by Doug Lea (dl@rocky.oswego.edu)

# This file is part of GNU CC.

# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY.  No author or distributor
# accepts responsibility to anyone for the consequences of using it
# or for whether it serves any particular purpose or works at all,
# unless he says so in writing.  Refer to the GNU CC General Public
# License for full details.

# Everyone is granted permission to copy, modify and redistribute
# GNU CC, but only under the conditions described in the
# GNU CC General Public License.   A copy of this license is
# supposed to have been given to you along with GNU CC so you
# can know your rights and responsibilities.  It should be in a
# file named COPYING.  Among other things, the copyright notice
# and this notice must be preserved on all copies.

srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = `if [ -d ../../binutils ] ; \
	then echo ../../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -d ../../binutils ] ; \
	then echo ../../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -d ../../texinfo/C ] ; \
	then echo ../../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

IO_DIR = iostream

CC = `if [ -d ../../../gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -d ../../../gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo gcc ; fi`

CFLAGS = -g
CXXFLAGS = -g -O
XTRAFLAGS = `if [ -d ../../../gcc ] ; \
	then echo -I../../../gcc/include ; \
	else echo ; fi`

LIBS = ../../libg++.a
WRAP_C_INCLUDES = -I$(srcdir)/../../g++-include

#### host and target dependent Makefile fragments come in here.
INSTALL = install -c
INSTALL_DATA = install -c -m 644
# RANLIB = ranlib
# Use -nostdinc++ flag if your version of gcc understands it.
NOSTDINC = # -nostdinc++

# If the C include files are C++-ready (with extern "C"),
# define: HAVE_CPLUS_EXTERN = 1 and: WRAP_C_INCLUDES =
# If not, define HAVE_CPLUS_EXTERN = 0, and do not define WRAP_C_INCLUDES here.
HAVE_CPLUS_EXTERN = 0

# Flags to pass to gen-params when building _G_config.h.
# For example: G_CONFIG_ARGS = size_t="unsigned long"
G_CONFIG_ARGS =
##

.SUFFIXES: .o .cc

.cc.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) -I. -I$(srcdir) -I../.. \
	  -I$(srcdir)/../../$(IO_DIR) -I$(srcdir)/../../src \
	  $(WRAP_C_INCLUDES) $(XTRAFLAGS) -c $<

.PHONY: all
all:

check: fib
	-./fib 1000

fib: fib.o
	$(CXX) -o fib fib.o $(LIBS)

.PHONY: clean
clean: force
	rm -f *.o *~ \#* fib

realclean: clean
	rm -f $(TEST_PROGS)

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status


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
# Copyright (C) 1989, 1992 Free Software Foundation, Inc.
# written by Douglas C. Schmidt (schmidt@ics.uci.edu)
# 
# This file is part of GNU GPERF.
# 
# GNU GPERF is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# GNU GPERF is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU GPERF; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 

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

AR = `if [ -f ../../../binutils/ar ] ; \
	then echo ../../../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -f ../../../binutils/ranlib ] ; \
	then echo ../../../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -f ../../../texinfo/C/makeinfo ] ; \
	then echo ../../../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

CC = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo gcc ; fi`

CXXFLAGS = -g -O

XTRAFLAGS = `if [ -f ../../../gcc/gcc ] ; \
	then echo -I../../../gcc/include ; \
	else echo ; fi`

LIBS = ../../libg++.a

IO_DIR = iostream

OBJECTS  = new.o options.o iterator.o main.o gen-perf.o key-list.o list-node.o\
           hash-table.o bool-array.o read-line.o std-err.o version.o 

DEPEND_SOURCES = ${srcdir}/*.cc
WRAP_C_INCLUDES = -I$(srcdir)/../../g++-include

PROGRAM = gperf
CFLAGS = -g

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

.SUFFIXES: .cc .o

.cc.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) $(HDEFINES) -DLO_CAL \
	-I../.. -I$(srcdir) -I$(srcdir)/../../$(IO_DIR) -I$(srcdir)/../../src \
	$(WRAP_C_INCLUDES) $(XTRAFLAGS) -c $<

.PHONY: all
all: $(PROGRAM)

.PHONY: info
info:
.PHONY: clean-info
clean-info:
.PHONY: install-info
install-info:

.PHONY: check
check:

gperf: $(OBJECTS) 
	$(CXX) -o $@ $(OBJECTS) $(LIBS)

.PHONY: install
install: $(PROGRAM)
	-parent=`echo $(bindir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(bindir) ] ; then true ; else mkdir $(bindir) ; fi
	$(INSTALL) $(PROGRAM) $(bindir)/$(PROGRAM)

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o core
clean: mostlyclean
distclean: clean
	rm -f $(PROGRAM) *~ Makefile config.status
realclean: distclean
	rm -f depend

.PHONY: force
force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status


# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

bool-array.o : bool-array.cc \
  options.h \
  std-err.h \
  trace.h \
  bool-array.h 
gen-perf.o : gen-perf.cc \
  options.h \
  std-err.h \
  trace.h \
  gen-perf.h \
  key-list.h \
  list-node.h \
  vectors.h \
  read-line.h \
  bool-array.h
hash-table.o : hash-table.cc \
  hash-table.h \
  list-node.h \
  std-err.h \
  vectors.h \
  options.h \
  trace.h
iterator.o : iterator.cc \
  $(srcdir)/../../$(IO_DIR)/stream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  iterator.h \
  trace.h
key-list.o : key-list.cc \
  options.h \
  std-err.h \
  trace.h \
  read-line.h \
  hash-table.h \
  list-node.h \
  vectors.h \
  key-list.h
list-node.o : list-node.cc \
  options.h \
  std-err.h \
  trace.h \
  list-node.h \
  vectors.h
main.o : main.cc \
  std-err.h \
  options.h \
  trace.h \
  gen-perf.h \
  key-list.h \
  list-node.h \
  vectors.h \
  read-line.h \
  bool-array.h 
new.o : new.cc \
  std-err.h \
  trace.h
options.o : options.cc \
  options.h \
  std-err.h \
  trace.h \
  iterator.h
read-line.o : read-line.cc \
  std-err.h \
  read-line.h \
  trace.h
std-err.o : std-err.cc \
  std-err.h \
  trace.h
version.o : version.cc 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY

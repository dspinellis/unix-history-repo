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
# This file is part of GNU TRIE-GEN.
# 
# GNU TRIE-GEN is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# GNU trie-gen is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU trie-gen; see the file COPYING.  If not, write to
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

AR = `if [ -f ../../binutils/ar ] ; \
	then echo ../../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -f ../../binutils/ranlib ] ; \
	then echo ../../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -f ../../texinfo/C/makeinfo ] ; \
	then echo ../../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

IO_DIR = iostream

CC = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo gcc ; fi`

CFLAGS = -g
CXXFLAGS = -g -O
XTRAFLAGS = `if [ -f ../../../gcc/gcc ] ; \
	then echo -I../../../gcc/include ; \
	else echo ; fi`

LIBS = ../../libg++.a
WRAP_C_INCLUDES = -I$(srcdir)/../../g++-include

.SUFFIXES: .o .cc

.cc.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) -I. -I$(srcdir) -I../.. \
	-I$(srcdir)/../../$(IO_DIR) -I$(srcdir)/../../src \
	$(WRAP_C_INCLUDES) $(XTRAFLAGS) -c $<

OBJS  = trie.o compact.o main.o version.o options.o
DEPEND_SOURCES = $(srcdir)/*.cc

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

.PHONY: all
all:
info:
clean-info:
install-info:

trie-gen: $(OBJS) 
	$(CXX) -o $@ $(OBJS) $(LIBS)

out.cc: trie-gen
	@echo "Generating a compacted minimal-prefix trie for files in /bin"
	/bin/ls -1 /bin | ./trie-gen -c > out.cc

check: out.o test.o
	$(CXX) out.o test.o $(LIBS)
	/bin/ls -1 /bin | ./a.out -v

run_tests: check

distrib: 
	(cd ..; rm -f trie-gen.tar.Z; tar cvf trie-gen.tar trie-gen; \
                compress trie-gen.tar)

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o \#* core trie-gen out.cc a.out
clean: mostlyclean
distclean: clean
	rm -f *~ config.status Makefile
realclean: distclean
	rm -f depend

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

compact.o : $(srcdir)/compact.cc $(srcdir)/compact.h 
main.o : $(srcdir)/main.cc $(srcdir)/options.h $(srcdir)/trie.h $(srcdir)/compact.h 
options.o : $(srcdir)/options.cc $(srcdir)/options.h 
trie.o : $(srcdir)/trie.cc $(srcdir)/trie.h $(srcdir)/compact.h 
version.o : $(srcdir)/version.cc 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY



# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

compact.o : compact.cc \
  compact.h
main.o : main.cc \
  $(srcdir)/../../$(IO_DIR)/stream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  options.h \
  trie.h \
  compact.h
options.o : options.cc \
  options.h
test.o : test.cc 
trie.o : trie.cc \
  options.h \
  trie.h \
  compact.h
version.o : version.cc \
  version.cc

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY

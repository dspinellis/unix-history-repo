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
host_makefile_frag = ../m68k-hp-bsd.mh
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

# 
# declarations from here on should not normally need to be changed 
# in order to compile libg++.a
#

# set in site specific makefile 
# Flags to compile the objects of a shared library
DYN_SHLFLAGS=

# library sources 

EH_FILES =

STREAM_OBJS = File.o  ostream.o istream.o streambuf.o filebuf.o Filebuf.o \
 PlotFile.o  SFile.o

# Comment out if you're getting regex from somewhere else.
REGEX_OBJ=regex.o
OBJS =  AllocRing.o Obstack.o builtin.o \
 $(REGEX_OBJ) Regex.o String.o  Integer.o Rational.o Complex.o Random.o \
 BitSet.o BitString.o LogNorm.o SmplHist.o SmplStat.o \
 Normal.o NegExp.o Weibull.o Erlang.o DiscUnif.o \
 Uniform.o Poisson.o HypGeom.o Geom.o Binomial.o \
 RNG.o ACG.o MLCG.o  RndInt.o  \
 Fix.o Fix16.o Fix24.o CursesW.o GetOpt.o $(EH_FILES) \
 new.o chr.o dtoa.o error.o gcd.o hash.o \
 lg.o fmtq.o ioob.o pow.o sqrt.o str.o timer.o \
 math.o compare.o \
 SLList.o DLList.o

DEPEND_SOURCES = $(srcdir)/*.cc

TARGETLIB = libg++.a

IO_DIR = iostream

CC = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo gcc ; fi`

XTRAFLAGS = `if [ -f ../../gcc/gcc ] ; \
	then echo -I../../gcc/include ; \
	else echo ; fi`
WRAP_C_INCLUDES=-I$(srcdir)/../g++-include

#### host, target, and site dependent Makefile fragments come in here.
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

CFLAGS = -g
CXXFLAGS = -g -O

.SUFFIXES: .cc .o

# ../libg++.a depends on the stamp file.
.cc.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) -I.. -I$(srcdir) -I$(srcdir)/../$(IO_DIR) $(WRAP_C_INCLUDES) $(XTRAFLAGS) -c $<
	@touch stamp


.PHONY: all
all: $(OBJS)
#all: $(TARGETLIB)

.PHONY: check
check:

info:
clean-info:
install-info:

#$(TARGETLIB): $(OBJS)
#	$(AR) $(AR_FLAGS) $(TARGETLIB) $(OBJS)
#	$(RANLIB) $(TARGETLIB)

libg++.so: $(OBJS) 
	rm -f libg++.so
	$(MKSHLIB) -f$(PIC) -o $@ $(OBJS) -lcurses -ltermcap -lm

#.PHONY: mostlyclean clean distclean realclean

mostlyclean:
	rm -f *.o stamp
clean: mostlyclean
	rm -f  *.a *.so
distclean: clean
#	rm -f *~ *.a config.status Makefile
	rm -f *~ *.a config.status
#realclean: distclean
#	rm -f depend

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

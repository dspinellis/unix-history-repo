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
# A makefile for the stuff now in libg++/etc

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

DEPEND_SOURCES = $(srcdir)/*.cc
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

.PHONY: check
check: tPlotFile3D
	./tPlotFile3D
	@echo use plot to look at the plot file test.pl

tPlotFile3D: PlotFile3D.o tPlotFile3D.o
	$(CXX) tPlotFile3D.o PlotFile3D.o $(LIBS) -lm -o $@

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o \#* core tPlotFile3D
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

PlotFile3D.o : PlotFile3D.cc \
  PlotFile3D.h \
  $(srcdir)/../../$(IO_DIR)/PlotFile.h \
  $(srcdir)/../../$(IO_DIR)/fstream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  Vec3D.h
tPlotFile3D.o : tPlotFile3D.cc \
  PlotFile3D.h \
  $(srcdir)/../../$(IO_DIR)/PlotFile.h \
  $(srcdir)/../../$(IO_DIR)/fstream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  Vec3D.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY

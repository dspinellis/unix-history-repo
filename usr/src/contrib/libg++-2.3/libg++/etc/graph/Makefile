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
PROTODIR = $(srcdir)/../../src/gen

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
check: graph $(srcdir)/test.dat $(srcdir)/test2.dat
	./graph -d -c 3 -g 2 -l "a diagonal line" -X "x axis" -Y "y axis" -m 1\
	        <$(srcdir)/test2.dat >test2.pl
	./graph -d <$(srcdir)/test.dat >test.pl
	@echo use plot to look at the plot files test.pl and test2.pl
run_tests: check

OBJECTS = pPlex.o pXPlex.o eGetOpt.o ePlotFile.o read_data.o tick_intrvl.o
DEPEND_SOURCES = $(srcdir)/*.cc

libgraph.a : $(OBJECTS)
	rm -f libgraph.a
	$(AR) $(AR_FLAGS) libgraph.a $(OBJECTS)
	$(RANLIB) libgraph.a

graph : graph.o libgraph.a
	$(CXX) graph.o libgraph.a $(LIBS) -lm -o $@

graph.o: pXPlex.h pPlex.h

pXPlex.h pXPlex.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; ../../genclass/genclass point val XPlex p
pPlex.h pPlex.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; ../../genclass/genclass point val Plex p

# GNU tail doesn't handle -r - use the 'tac' program instead.
REVERSE_LINES = tail -r
#REVERSE_LINES = tac

$(srcdir)/test.dat:
	-echo 0 0 1 1 2 0 | spline | $(REVERSE_LINES) > $(srcdir)/test.dat

$(srcdir)/test2.dat:
	echo 0 0 >$(srcdir)/test2.dat
	echo 1 1 "label for 1 1" >>$(srcdir)/test2.dat
	echo 2 2 >>$(srcdir)/test2.dat
	echo 3 3 "label for 3 3" >>$(srcdir)/test2.dat
	echo 4 4 >>$(srcdir)/test2.dat

DIST = Makefile graph.tex ChangeLog \
eGetOpt.cc		read_data.cc \
eGetOpt.h		read_data.h \
ePlotFile.cc		tick_intrvl.cc \
ePlotFile.h		tick_intrvl.h \
graph.cc		pdefs.h

graph.tar.Z : $(DIST) CHECKSUMS
	tar cfz $@ $^ CHECKSUMS

CHECKSUMS : $(DIST)
	sum $(DIST) >CHECKSUMS

dist : graph-dist.tar.Z

graph-dist.tar.Z : $(DIST)
	-rm -rf graph-dist
	mkdir graph-dist
	ln $(DIST) graph-dist
	tar cfz graph-dist.tar.Z $(DIST)
	rm -rf graph-dist

graph.shar : pdefs.h graph.cc Makefile graph.texinfo
	shar  pdefs.h graph.cc Makefile graph.texinfo > $@

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o \#* core  pXPlex* pPlex* test.pl test2.pl
clean: mostlyclean
	rm -f graph graph.shar libgraph.a 
distclean: clean
	rm -f *~ config.status Makefile
realclean: distclean
	rm -f depend

.PHONY: check graph_all all graph run_tests dist

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status



# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

eGetOpt.o : eGetOpt.cc \
  eGetOpt.h \
  $(srcdir)/../../$(IO_DIR)/stream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h 
ePlotFile.o : ePlotFile.cc \
  ePlotFile.h \
  $(srcdir)/../../$(IO_DIR)/PlotFile.h \
  $(srcdir)/../../$(IO_DIR)/fstream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h 
graph.o : graph.cc \
  read_data.h \
  $(srcdir)/../../$(IO_DIR)/stream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  pXPlex.h \
  eGetOpt.h \
  ePlotFile.h \
  $(srcdir)/../../$(IO_DIR)/PlotFile.h \
  $(srcdir)/../../$(IO_DIR)/fstream.h \
  tick_intrvl.h \
  $(srcdir)/../../$(IO_DIR)/strstream.h
read_data.o : read_data.cc \
  read_data.h \
  $(srcdir)/../../$(IO_DIR)/stream.h \
  $(srcdir)/../../$(IO_DIR)/iostream.h \
  $(srcdir)/../../$(IO_DIR)/streambuf.h \
  pXPlex.h
tick_intrvl.o : tick_intrvl.cc \
  tick_intrvl.h 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY

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
###*#######################################################################
#
# Makefile to test the installation of g++ and libg++
# (by hgs@cygnus.com)
# 
# Usage: make foo_main test-90S test-90D PREFIX=prefix [other macros]

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

IO_DIR = iostream

CC = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo gcc ; fi`
LIBS = ../libg++.a

XTRAFLAGS = `if [ -f ../../gcc/gcc ] ; \
	then echo -I$(srcdir)/../../gcc/include ; \
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

#
# Compilation macros
#
PREFIX		= $(prefix)

CFLAGS		=
CXXFLAGS	=
CPP_FLAGS	= #-I$(srcdir)/../g++-include
OPTIMIZE_FLAGS	= -O
DEBUG_FLAGS	= -g -v #-Wall
COMPILE_FLAGS=$(NOSTDINC) -I.. -I$(srcdir) -I$(srcdir)/../src -I$(srcdir)/../$(IO_DIR) $(WRAP_C_INCLUDES) $(XTRAFLAGS) 

#
# Suffix rules
#
.SUFFIXES: .o .cc

.cc.o:
	$(CXX) $(CXXFLAGS) $(COMPILE_FLAGS) -c $<

DEPEND_SOURCES = $(srcdir)/*.cc

###*#######################################################################
#

T90  = $(T90S) $(T90D)
T90D =  # dex_bar

FLAGS_90 = \
 GXX="$(GXX)"\
 CPP_FLAGS="$(CPP_FLAGS)"\
 OPTIMIZE_FLAGS="$(OPTIMIZE_FLAGS)"\
 DEBUG_FLAGS="$(DEBUG_FLAGS)"\
 GXXLDFLAGS="$(GXXLDFLAGS)"

.PHONY: all
all: foo_main

.PHONY: check
check:
.PHONY: info
info:
.PHONY: install-info
install-info:
.PHONY: clean-info
clean-info:

help:
	@echo Usage: make foo_main test-40 test-90S test-90D PREFIX=prefix [other macros]


test-90S: foo_main a b

test-90D: dfoo_main da db

test: $(TEST)
	for P in $(TEST) ; do if [ $$P != foo_main ]; then eval ./$$P; fi; done

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o foo_main a b dfoo_main da db core *foo_main.out *foo_main.diff *foo_main.run
clean: mostlyclean
distclean: clean
	rm -f *~ config.status Makefile $(TRASH_FILES)
realclean: distclean
	rm -f depend

a : a.o
	$(CXX) -o a  a.o $(LIBS)
da : a.o
	$(CXX) -o da -dynamic  a.o $(LIBS)


b : bf.o bm.o
	$(CXX) -o b bm.o bf.o $(LIBS)

db : bf.o bm.o
	$(CXX) -o db -dynamic bm.o bf.o $(LIBS)


foo_main:   foo_main.diff
	@true

dfoo_main: dfoo_main.diff
	@true

foo_main.run: foo_main.o foo_func.o Foo.o expected.out
	$(CXX) $(DEBUG_FLAGS) -o $@  foo_main.o foo_func.o Foo.o

foo_main.out: foo_main.run
	./foo_main.run > foo_main.out

foo_main.diff: foo_main.out $(srcdir)/expected.out
	diff $(srcdir)/expected.out foo_main.out >foo_main.diff

dfoo_main.run: foo_main.o foo_func.o Foo.o
	$(CXX) $(DEBUG_FLAGS) -o $@ -dynamic foo_main.o foo_func.o Foo.o $(LIBS)

dfoo_main.out: dfoo_main.run
	./dfoo_main.run > dfoo_main.out

dfoo_main.diff: dfoo_main.out $(srcdir)/expected.out
	diff $(srcdir)/expected.out dfoo_main.out

ex_bar.o: ex_bar.cc
	$(CXX) -fhandle-exceptions ex_bar.cc

ex_bar: ex_bar.o
	$(CXX) $(DEBUG_FLAGS) -o $@ ex_bar.o

dex_bar: ex_bar.o
	$(CXX) $(DEBUG_FLAGS) -o $@ -dynamic ex_bar.o

# --- Generate depend rules ---

SRCDIR=$(srcdir)/..

# The sed script below attempts to make the depend output portable by
# making the output use the same macros used elsewhere in the Makefile:
# - It replaces double // by a single /.
# - It replaces include files that match part of the GXX_INCLUDE_DIRS
#   by names defined in terms of the macros used to define GXX_INCLUDE_DIRS.
# - It removes any absolute include file names that remain.
# - then remove lines, which contain only `\'
#
depend: depend.tmp
	if [ "x$(DEPEND_SOURCES)" != "x" ] ; then \
	sed < depend.tmp \
	  -e 's|//|/|g' \
	  -e 's|$(srcdir)|$$(srcdir)|g' \
	  -e 's| /[^ ]*[.]h||g' \
	  -e 's|: *\$$(srcdir)/\(.*\.[cC]*\)|: \1|' \
	  -e '/^[ 	]*\\$$/d' -e 's/^[ 	]*$$//' \
	| awk 'BEGIN   { prev = "" } \
	      /^( )*$$/ { if (prev ~ /\\$$/) \
			 { prev = substr(prev,1,length(prev)-1); next } \
		       } \
                       { print prev; prev = $$0 } \
	       END { if (prev !~ /^( )*$$/) print prev }' \
	> $(srcdir)/depend ;\
	fi

depend.tmp: $(DEPEND_SOURCES) 
	if [ "x$(DEPEND_SOURCES)" = "x" ] ; then \
	  echo "" > $(srcdir)/depend ;\
	else \
	echo "" >depend.tmp ; \
	$(SRCDIR)/utils/g++dep.sh -f depend.tmp $(GXX_INCLUDE_DIRS) \
	  $(DEPEND_SOURCES) ;\
	fi

do_depend: depend
	if [ "$(SUBDIRS)" != "" ] ; then \
	for D in $(SUBDIRS) x ; do     		   \
		if [ -d $$D ] ; then 		   \
		echo "cd $$D; $(MAKE) do_depend" ; \
		(cd $$D ; $(MAKE) do_depend );	   \
		fi ;				   \
	done ;\
	fi
	

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

Foo.o : Foo.cc \
  $(srcdir)/Foo.h
a.o : a.cc 
bf.o : bf.cc 
bm.o : bm.cc 
ex_bar.o : ex_bar.cc \
  $(srcdir)/ex_bar.cc
foo_func.o : foo_func.cc \
  $(srcdir)/Foo.h
foo_main.o : foo_main.cc \
  $(srcdir)/Foo.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY

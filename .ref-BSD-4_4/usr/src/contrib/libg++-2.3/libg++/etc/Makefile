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

FLAGS_TO_PASS= \
	"prefix=$(prefix)" \
	"exec_prefix=$(exec_prefix)" \
	"tooldir=$(tooldir)" \
	"AR=$(AR)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"RANLIB=$(RANLIB)" \
	"LOADLIBES=$(LOADLIBES)" \
	"LDFLAGS=$(LDFLAGS)" \
	"MAKEINFO=$(MAKEINFO)" \
	"XTRAFLAGS=$(XTRAFLAGS)"

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

CC = `if [ -f $${rootprefix}../../gcc/gcc ] ; \
	then echo $${rootprefix}../../gcc/gcc -B$${rootprefix}../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f $${rootprefix}../../gcc/gcc ] ; \
	then echo $${rootprefix}../../gcc/gcc -B$${rootprefix}../../gcc/ ; \
	else echo gcc ; fi`
CFLAGS = -g
CXXFLAGS = -g -O

XTRAFLAGS = `if [ -f $${rootprefix}../../gcc/gcc ] ; \
	then echo -I$${rootprefix}../../gcc/include ; \
	else echo ; fi`

SUBDIRS= graph ADT-examples benchmarks PlotFile3D lf trie-gen fib

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
.PHONY: install
install:
info:
install-info:
clean-info:

check: force
	@rootprefix=`pwd`/; export rootprefix; \
	for i in $(SUBDIRS); do \
		echo "cd $$i; make check ..." ; \
		(cd $$i ; $(MAKE) check $(FLAGS_TO_PASS)) || exit 1 ; \
	done

.PHONY: mostlyclean clean distclean realclean
.PHONY: do_mostlyclean do_clean do_distclean do_realclean

do_mostlyclean:
	rm -f *.o core \#*
do_clean: do_mostlyclean
do_distclean: do_clean
	rm -f *~ *.a config.status Makefile
do_realclean: do_distclean
	rm -f depend

mostlyclean: do_mostlyclean
	@for D in $(SUBDIRS); do     					\
		echo "cd $$D; make mostlyclean ..." ;			\
		(cd $$D ; $(MAKE) mostlyclean );\
	done 
clean: do_clean
	@for D in $(SUBDIRS); do     					\
		echo "cd $$D; make clean ..." ;				\
		(cd $$D ; $(MAKE) clean TRASH_FILES="$(TRASH_FILES)");	\
	done 
distclean:
	@for D in $(SUBDIRS); do     					\
		echo "cd $$D; make distclean ..." ;			\
		(cd $$D ; $(MAKE) distclean TRASH_FILES="$(TRASH_FILES)");\
	done 
	@$(MAKE) do_distclean
realclean:
	@for D in $(SUBDIRS); do     					   \
		echo "cd $$D; make realclean ..." ;			   \
		(cd $$D ; $(MAKE) realclean TRASH_FILES="$(TRASH_FILES)"); \
	done
	@$(MAKE) do_realclean

force:
.PHONY: force check

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status


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
# Makefile for g++ library genclass test

# Copyright (C) 1988, 1992 Free Software Foundation

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
tooldir = $(libdir)/m68k-hp-bsd

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
gxx_includedir = $(libdir)/g++-include
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = `if [ -f $${rootme}/../binutils/ar ] ; \
	then echo $${rootme}/../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -f $${rootme}/../binutils/ranlib ] ; \
	then echo $${rootme}/../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -f $${rootme}/../texinfo/C/makeinfo ] ; \
	then echo $${rootme}/../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

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
all: genclass # gentest

.PHONY: info
info:
.PHONY: install-info
install-info:
.PHONY: clean-info
clean-info:

.PHONY: check
check: # gentest

genclass: $(srcdir)/genclass.sh
	echo "/^PROTODIR=/c\\" > sedscript
	echo "PROTODIR=$$\{PROTODIR-$(gxx_includedir)/gen\}" >> sedscript
	sed <../Makefile -n -e '/VERSION/s/LIBG++_DIST_VERSION *= *\(.*\)/s|<VERSION>|\1|/p' >> sedscript
	sed -f sedscript < $(srcdir)/genclass.sh > genclass.tmp
	chmod 0755 genclass.tmp
	rm -f sedscript
	mv genclass.tmp genclass

gentest: $(srcdir)/gentest.sh $(srcdir)/expected.out
	$(srcdir)/gentest.sh > my.out 2>&1
	diff $(srcdir)/expected.out my.out

.PHONY: install
install:
	-parent=`echo $(bindir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(bindir) ] ; then true ; else mkdir $(bindir) ; fi
	$(INSTALL) genclass $(bindir)/genclass

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
clean: mostlyclean
	rm -f $(TRASHFILES) genclass *.h *.cc my.out
distclean: clean
	rm -f *~ config.status Makefile
realclean: distclean

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

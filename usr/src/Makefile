#
# Copyright (c) 1986, 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.19 (Berkeley) %G%
#
# This makefile is designed to be run in one of two ways:
#
#	make build
#	make installsrc
# This is the most rigorous way to rebuild the source tree.  `make build'
# will clean the source tree, install the include files, remake the
# dependencies, and then compile and install the libraries and the compiler
# twice before building the rest of the sources.  The `make installsrc'
# will then install the remaining binaries.
#
#	make all
#	make install
# The `make all' (the default) will compile everything, and install
# nothing.  The `make install' will then install everything.
#
# C library options: passed to libc makefile.
# See usr.lib/libc/Makefile for explanation.
#
# HOSTLOOKUP must be either named or hosttable.
# DFLMON must be either mon.o or gmon.o.
# DEFS may include -DLIBC_SCCS, -DSYSLIBC_SCCS, both, or neither.
#
DEFS=		-DLIBC_SCCS
DFLMON=		mon.o
LIBCDEFS=	DFLMON=${DFLMON} DEFS="${DEFS}"

# global flags
# SRC_MFLAGS are used on makes in command source directories,
# but not in library or compiler directories that will be installed
# for use in compiling everything else.
#
DESTDIR=
CFLAGS=		-O
SRC_MFLAGS=	-k

SRCLIB=	lib
# order is important, old must be first, man must be last; contrib is
# too big for the current file system
# bin
SRC=	old games libexec pgrm sbin share usr.bin usr.sbin local man
ALL=	${SRCLIB} ${SRC}

all: ${ALL}

build: cleandir build1 ${SRC}

${ALL}: FRC
	@echo; echo +++ compiling $@
	cd $@; make ${MFLAGS}

build1:
	@echo +++ installing includes
	cd include; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo +++ building dependencies
	@for i in ${ALL}; do \
		echo; echo +++ dependencies for $$i; \
		(cd $$i; make ${MFLAGS} depend); \
	done
	@echo +++ compiling C compiler
	cd usr.bin/cpp; make ${MFLAGS}
	cd usr.libexec/c2; make ${MFLAGS}
	cd usr.libexec/pcc; make ${MFLAGS}
	@echo
	@echo +++ installing C compiler
	cd usr.bin/cpp; make ${MFLAGS} install
	cd usr.libexec/c2; make ${MFLAGS} install
	cd usr.libexec/pcc; make ${MFLAGS} install
	@echo
	@echo +++ compiling C library
	cd usr.lib/libc; make ${MFLAGS} ${LIBCDEFS}
	@echo
	@echo +++ installing C library
	cd usr.lib/libc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo +++ cleaning C compiler
	cd usr.bin/cpp; make ${MFLAGS} clean
	cd usr.libexec/c2; make ${MFLAGS} clean
	cd usr.libexec/pcc; make ${MFLAGS} clean
	@echo
	@echo +++ re-compiling C compiler
	cd usr.bin/cpp; make ${MFLAGS}
	cd usr.libexec/c2; make ${MFLAGS}
	cd usr.libexec/pcc; make ${MFLAGS}
	@echo
	@echo +++ re-installing C compiler
	cd usr.bin/cpp; make ${MFLAGS} install
	cd usr.libexec/c2; make ${MFLAGS} install
	cd usr.libexec/pcc; make ${MFLAGS} install
	@echo
	@echo +++ cleaning C library
	cd usr.lib/libc; make ${MFLAGS} clean
	@echo
	@echo +++ compiling all libraries
	cd usr.lib; make ${MFLAGS} ${LIBCDEFS}
	@echo
	@echo +++ installing all libraries
	cd usr.lib; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo +++ libraries done
	@echo

installsrc: FRC
	@for i in ${SRC}; do \
		echo; \
		echo +++ installing $$i; \
		(cd $$i; \
		    make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

clean: FRC
	@for i in ${ALL}; do \
		echo; \
		echo +++ cleaning $$i; \
		(cd $$i; make ${SRC_MFLAGS} clean); \
	done

cleandir: FRC
	@for i in include ${ALL}; do \
		echo; echo '+++ cleaning (cleandir)' $$i; \
		(cd $$i; make ${SRC_MFLAGS} cleandir); \
	done

depend: FRC
	@for i in ${ALL}; do \
		echo; \
		echo +++ doing dependencies for $$i; \
		(cd $$i; make ${SRC_MFLAGS} depend); \
	done

install: FRC
	@for i in ${ALL}; do \
		echo; \
		echo +++ installing $$i; \
		(cd $$i; \
		    make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

tags: FRC
	@for i in include lib usr.lib; do \
		echo; \
		echo +++ making tags for $$i; \
		(cd $$i; make ${SRC_MFLAGS} TAGSFILE=../tags tags); \
	done
	sort -u +0 -1 -o tags tags

FRC:

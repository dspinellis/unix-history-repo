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
#	@(#)Makefile	4.18 (Berkeley) %G%
#
# This makefile is designed to be run in one of three ways:
#
#	make clean
#	make depend
#	make build
#	make installsrc
# This is the most rigorous way to rebuild the source tree.
# The first two steps will clean the source tree and remake the dependencies.
# The `make build' will compile and install the libraries and the compiler
# twice before building the rest of the sources.  The `make installsrc' will
# then install the remaining binaries.
#
#	make libthenall
#	make installsrc
# The `make libthenall' will compile and install the libraries, compile
# and install the compiler and then compile everything else.  Note,
# however, that the libraries will have been built with the old compiler.
# The `make installsrc' will then install the remaining binaries.
#
#	make all
#	make install
# The `make all' (the default) will compile everything, and install
# nothing.  The `make install' will then install everything.
#
# C library options: passed to libc makefile.
# See lib/libc/Makefile for explanation.
#
# HOSTLOOKUP must be either named or hosttable.
# DFLMON must be either mon.o or gmon.o.
# DEFS may include -DLIBC_SCCS, -DSYSLIBC_SCCS, both, or neither.
#
DEFS=		-DLIBC_SCCS
DFLMON=		mon.o
HOSTLOOKUP=	named
LIBCDEFS=	HOSTLOOKUP=${HOSTLOOKUP} DFLMON=${DFLMON} DEFS="${DEFS}"

# global flags
# SRC_MFLAGS are used on makes in command source directories,
# but not in library or compiler directories that will be installed
# for use in compiling everything else.
#
DESTDIR=
CFLAGS=		-O
SRC_MFLAGS=	-k

LIBDIR=	lib usr.lib
# order is important, old and man must be #1 and #2
SRCDIR=	old man bin usr.bin etc ucb new games local

all: ${LIBDIR} ${SRCDIR}

libthenall: buildlib1 buildlib3 src

build: buildlib1 buildlib2 buildlib3 src

src: ${SRCDIR}

lib: FRC
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	cd lib; make ${MFLAGS} pcc cpp c2

usr.lib ${SRCDIR}: FRC
	@echo
	@echo === compiling $@
	cd $@; make ${MFLAGS} ${SRC_MFLAGS}

buildlib1: FRC
	@echo === installing /usr/include
	cd include; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo === compiling libc.a
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	@echo === installing /lib/libc.a
	cd lib/libc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo === compiling C compiler
	cd lib; make ${MFLAGS} pcc cpp c2
	@echo
	@echo === installing C compiler
	cd lib/pcc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/cpp; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/c2; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo

buildlib2: FRC
	@echo === cleaning lib
	cd lib; make ${MFLAGS} clean
	@echo === re-compiling libc.a
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	@echo === re-installing /lib/libc.a
	cd lib/libc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo === re-compiling C compiler
	cd lib; make ${MFLAGS} pcc cpp c2
	@echo === re-installing C compiler
	cd lib/pcc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/cpp; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/c2; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo

buildlib3: FRC
	@echo === compiling usr.lib
	cd usr.lib; make ${MFLAGS} ${SRC_MFLAGS}
	@echo === installing /usr/lib
	cd usr.lib; make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install
	@echo === done with libraries
	@echo

install: FRC
	-for i in ${LIBDIR} ${SRCDIR}; do \
		echo; \
		echo === installing $$i; \
		(cd $$i; \
		make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

installsrc: FRC
	-for i in ${SRCDIR}; do \
		echo; \
		echo === installing $$i; \
		(cd $$i; \
		make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

depend: FRC
	for i in ${LIBDIR} ${SRCDIR}; do \
		echo; \
		echo === doing dependencies for $$i; \
		(cd $$i; make ${MFLAGS} depend); \
	done

tags: FRC
	for i in include lib usr.lib; do \
		echo; \
		echo === making tags for $$i; \
		(cd $$i; make ${MFLAGS} TAGSFILE=../tags tags); \
	done
	sort -u +0 -1 -o tags tags

clean: FRC
	for i in ${LIBDIR} ${SRCDIR}; do \
		echo; \
		echo === cleaning $$i; \
		(cd $$i; make -k ${MFLAGS} clean); \
	done

FRC:

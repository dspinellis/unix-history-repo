#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.15	(Berkeley)	%G%
#
# This makefile is designed to be run in one of three ways:
#
#	make build
#	make installsrc
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
# HOSTLOOKUP must be either named or hosttable.
# DFLMON must be either mon.o or gmon.o.
# DEFS may include -DLIBC_SCCS, -DSYSLIBC_SCCS, both, or neither.
#
HOSTLOOKUP=named
DFLMON=mon.o
DEFS= -DLIBC_SCCS
MACHINE=tahoe
LIBCDEFS= MACHINE=${MACHINE} HOSTLOOKUP=${HOSTLOOKUP} DFLMON=${DFLMON} DEFS="${DEFS}"

# global flags
# SRC_MFLAGS are used on makes in command source directories,
# but not in library or compiler directories that will be installed
# for use in compiling everything else.
#
DESTDIR=
CFLAGS=	-O
SRC_MFLAGS = -k

# Programs that live in subdirectories, and have makefiles of their own.
#
LIBDIR= lib usr.lib
SRCDIR=	bin usr.bin etc ucb new games local old

all: ${LIBDIR} ${SRCDIR}

libthenall: buildlib1 buildlib3 ${SRCDIR}

build: buildlib1 buildlib2 buildlib3 ${SRCDIR}

lib:	FRC
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	cd lib; make ${MFLAGS} pcc cpp c2

usr.lib ${SRCDIR}: FRC
	cd $@; make ${MFLAGS} ${SRC_MFLAGS}

buildlib1: FRC
	@echo installing /usr/include
	cd include; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo compiling libc.a
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	@echo installing /lib/libc.a
	cd lib/libc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo compiling C compiler
	cd lib; make ${MFLAGS} pcc cpp c2
	@echo installing C compiler
	cd lib/pcc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/cpp; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/c2; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo

buildlib2: FRC
	cd lib; make ${MFLAGS} clean
	@echo re-compiling libc.a
	cd lib/libc; make ${MFLAGS} ${LIBCDEFS}
	@echo re-installing /lib/libc.a
	cd lib/libc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo
	@echo re-compiling C compiler
	cd lib; make ${MFLAGS} pcc cpp c2
	@echo re-installing C compiler
	cd lib/pcc; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/cpp; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib/c2; make ${MFLAGS} DESTDIR=${DESTDIR} install
	@echo

buildlib3: FRC
	@echo compiling usr.lib
	cd usr.lib; make ${MFLAGS} ${SRC_MFLAGS}
	@echo installing /usr/lib
	cd usr.lib; make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install
	@echo

FRC:

install:
	-for i in ${LIBDIR} ${SRCDIR}; do \
		(cd $$i; \
		make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

installsrc:
	-for i in ${SRCDIR}; do \
		(cd $$i; \
		make ${MFLAGS} ${SRC_MFLAGS} DESTDIR=${DESTDIR} install); \
	done

tags:
	for i in include lib usr.lib; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../tags tags); \
	done
	sort -u +0 -1 -o tags tags

clean:
	rm -f a.out core *.s *.o
	for i in ${LIBDIR} ${SRCDIR}; do (cd $$i; make -k ${MFLAGS} clean); done

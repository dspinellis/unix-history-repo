#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3.1.1 (Berkeley) %G%
#
# Bug report processor and associated programs
#

BUGS_NAME = 4bsd-bugs
BUGS_HOME = @ucbarpa.BERKELEY.EDU
DEFS = -DBUGS_NAME='"$(BUGS_NAME)"' -DBUGS_HOME='"$(BUGS_HOME)"'
CFLAGS = -O -DUNIXCOMP $(DEFS)
LIBDIR=	/usr/lib
DESTDIR=

all: bugfiler unixtomh

bugfiler: bugfiler.c
	${CC} ${CFLAGS} -o bugfiler bugfiler.c

unixtomh: unixtomh.c
	${CC} ${CFLAGS} -o unixtomh unixtomh.c

install: bugfiler sendbug.sh bugformat
	install -s bugfiler ${DESTDIR}${LIBDIR}/bugfiler
	install -c sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -c -m 644 bugformat ${DESTDIR}/usr/ucb/bugformat

clean:
	rm -f *.o bugfiler

#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) 4/24/86
#
# Bug report processor and associated programs
#

BUGS_NAME = 4bsd-bugs
BUGS_HOME = @ucbvax.BERKELEY.EDU
DEFS = -DBUGS_NAME='"$(BUGS_NAME)"' -DBUGS_HOME='"$(BUGS_HOME)"'
CFLAGS = -O -DUNIXCOMP $(DEFS)
LIBDIR=	/usr/lib
DESTDIR=

all: bugfiler unixtomh

bugfiler: bugfiler.c
	${CC} ${CFLAGS} -DUNIXTOMH='"${LIBDIR}/unixtomh"' -o bugfiler bugfiler.c

unixtomh: unixtomh.c
	${CC} ${CFLAGS} -o unixtomh unixtomh.c

install: bugfiler sendbug.sh bugformat unixtomh
	install -s bugfiler ${DESTDIR}${LIBDIR}/bugfiler
	install -c sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -s unixtomh ${DESTDIR}${LIBDIR}/unixtomh
	install -c -m 644 bugformat ${DESTDIR}/usr/lib/bugformat

clean:
	rm -f *.o bugfiler unixtomh

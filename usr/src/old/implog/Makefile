# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
ALL=	implog implogd
DESTDIR=
CFLAGS=-O

all:	${ALL}

implog:	implog.o
	${CC} implog.o -o implog

implogd: implogd.o
	${CC} implogd.o -o implogd

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: ${ALL}
	install implog ${DESTDIR}/etc
	install implogd ${DESTDIR}/etc

#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.9 (Berkeley) %G%
#
# Bug report processor and associated programs
#

CFLAGS = -O -I.
DESTDIR=
SRCS=	bugfiler.c error.c gethead.c process.c redist.c reply.c
OBJS=	bugfiler.o error.o gethead.o process.o redist.o reply.o

all: bugfiler

bugfiler: ${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS}

install: bugfiler sendbug.sh bugformat
	install -s -m 4755 -o root -g staff bugfiler ${DESTDIR}/usr/lib/bugfiler
	install -c sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -c -m 644 bugformat ${DESTDIR}/usr/lib/bugformat

clean:
	rm -f ${OBJS} core bugfiler

depend:

tags:
	ctags ${SRCS}

lint:
	lint ${CFLAGS} -chapbx ${SRCS}

${OBJS}: bug.h

#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.13 (Berkeley) %G%
#
CFLAGS=	-O -I.
LIBC=	/lib/libc.a
SRCS=	bugfiler.c error.c gethead.c process.c redist.c reply.c
OBJS=	bugfiler.o error.o gethead.o process.o redist.o reply.o
MAN1=	sendbug.0
MAN8=	bugfiler.0

all: bugfiler

bugfiler: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean:
	rm -f ${OBJS} core bugfiler

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN1} ${MAN8}
	install -s -o root -g bin -m 4755 bugfiler ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 sendbug.sh ${DESTDIR}/usr/ucb
	install -c -o bin -g bin -m 644 bugformat ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 444 ${MAN1} ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 ${MAN8} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

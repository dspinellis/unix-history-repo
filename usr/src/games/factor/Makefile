#
# Copyright (c) 1989 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
FSRCS=	factor.c pr_tbl.c
FOBJS=	factor.o pr_tbl.o
PSRCS=	primes.c pr_tbl.c pattern.c
POBJS=	primes.o pr_tbl.o pattern.o
MAN=	factor.0

all: factor primes

factor: ${LIBC} ${FOBJS}
	${CC} -o $@ ${CFLAGS} ${FOBJS}

primes: ${LIBC} ${POBJS}
	${CC} -o $@ ${CFLAGS} ${POBJS} -lm

clean:
	rm -f ${FOBJS} ${POBJS} core factor primes

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 factor primes ${DESTDIR}/usr/games
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6
	rm -f ${DESTDIR}/usr/man/cat6/primes.0
	ln ${DESTDIR}/usr/man/cat6/${MAN} ${DESTDIR}/usr/man/cat6/primes.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

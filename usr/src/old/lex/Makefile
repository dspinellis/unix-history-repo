#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	4.4 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	lmain.c parser.c sub1.c sub2.c header.c
OBJS=	lmain.o parser.o sub1.o sub2.o header.o
MAN=	lex.0

all: lex

lex: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} -o $@

smallex: ${SRCS} ${LIBC} FRC
	${CC} -DSMALL ${CFLAGS} ${SRCS} -o $@

clean:
	rm -f ${OBJS} core lex smallex parser.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 lex ${DESTDIR}/usr/bin/lex
	install -c -o bin -g bin -m 755 ncform ${DESTDIR}/usr/lib/lex/ncform
	install -c -o bin -g bin -m 755 nrform ${DESTDIR}/usr/lib/lex/nrform
	install -c -o bin -g bin -m 755 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

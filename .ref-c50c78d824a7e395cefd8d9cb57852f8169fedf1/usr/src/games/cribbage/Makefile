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
#	@(#)Makefile	5.8 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
HDRS=	cribbage.h deck.h cribcur.h
SRCS=	extern.c crib.c support.c cards.c score.c io.c
OBJS=	extern.o crib.o support.o cards.o score.o io.o
TOBJS=	test.o cards.o score.o io.o extern.o
MAN=	cribbage.0

all: cribbage crib.instr

cribbage: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

test: ${TOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${TOBJS} ${LIBS}

crib.instr: cribbage.n macro
	nroff cribbage.n > crib.instr

clean:
	rm -f ${OBJS} core crib.instr cribbage test

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -o games -g bin -m 400 crib.instr ${DESTDIR}/usr/games/lib/crib.instr
	install -s -o games -g bin -m 4700 cribbage ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f cribbage; ln -s dm cribbage; chown games.bin cribbage)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/games/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

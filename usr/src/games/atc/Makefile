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
#	@(#)Makefile	5.3 (Berkeley) %G%
#
CFLAGS=	-O -DBSD
YFLAGS=	-d
LIBC=	/lib/libc.a
LIBS=	-ll -lm -lcurses -ltermcap
SRCS=	extern.c grammar.c graphics.c input.c lex.c list.c log.c \
	main.c tunable.c update.c
OBJS=	extern.o grammar.o input.o lex.o list.o log.o main.o tunable.o \
	graphics.o update.o
MAN=	atc.0

all: atc

atc: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} ${LIBS} -o $@

clean:
	rm -f ${OBJS} core atc grammar.c y.tab.h y.tab.c lex.yy.c lex.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/atc ] || mkdir ${DESTDIR}/usr/games/lib/atc
	chmod 700 ${DESTDIR}/usr/games/lib/atc
	chown games.bin ${DESTDIR}/usr/games/lib/atc
	(cd games; install -c -o games -g bin -m 600 * ${DESTDIR}/usr/games/lib/atc)
	install -s -o games -g bin -m 4700 atc ${DESTDIR}/usr/games/hide/atc
	(cd ${DESTDIR}/usr/games; rm -f atc; ln -s dm atc; chown games.bin atc)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/games/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

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
LIBS=	-lm -ltermcap
SRCS1=	snake.c move.c
OBJS1=	snake.o move.o
SRCS2=	snscore.c
OBJS2=	snscore.o
MAN=	snake.0

all: snake snscore

snake: ${OBJS1} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS1} ${LIBS}

snscore: ${OBJS2} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS2}

clean:
	rm -f ${OBJS1} ${OBJS2} core snake snscore

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS1} ${SRCS2}

install: ${MAN}
	install -s -o games -g bin -m 4700 snake ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f snake; ln -s dm snake; chown games.bin snake)
	install -s -o games -g bin -m 700 snscore ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f snscore; ln -s dm snscore; chown games.bin snscore)
	-[ -s ${DESTDIR}/usr/games/lib/snakerawscores ] || install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/snakerawscores
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS1}
	lint ${CFLAGS} ${SRCS2}

tags: ${SRCS}
	ctags ${SRCS1} ${SRCS2}

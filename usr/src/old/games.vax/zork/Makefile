#
# Copyright (c) 1988 The Regents of the University of California.
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
#	@(#)Makefile	4.4 (Berkeley) 10/20/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	zork.c
OBJS=
MAN=	zork.0

all: zork

zork: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f ${OBJS} core zork

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/zorklib ] || mkdir ${DESTDIR}/usr/games/lib/zorklib
	install -s -o bin -g bin -m 755 zork ${DESTDIR}/usr/games/zork
	install -c -o bin -g bin -m 755 zorklib/dtext.dat ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/dindex.dat ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/doverlay ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/dungeon ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

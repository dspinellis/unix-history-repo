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
#	@(#)Makefile	5.2 (Berkeley) 3/5/89
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	btlgammon.c
OBJS=

all: btlgammon

btlgammon: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS}

clean:
	rm -f ${OBJS} core btlgammon

cleandir: ${MAN}
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 4700 btlgammon ${DESTDIR}/usr/games/hide
	install -c -o games -g bin -m 444 backrules ${DESTDIR}/usr/games/lib
	(cd ${DESTDIR}/usr/games; rm -f btlgammon; ln -s dm btlgammon; chown games.bin btlgammon)

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

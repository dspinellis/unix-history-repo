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
#	@(#)Makefile	5.4 (Berkeley) %G%
#
# -DLOG		log games in /usr/adm
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dm.c
OBJS=
MAN=	dm.0 dm.config.0

all: dm

dm: ${LIBC}
	${CC} -o $@ ${CFLAGS} dm.c

clean:
	rm -f core dm

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g kmem -m 6755 dm ${DESTDIR}/usr/games
#	install -c -o games -g bin -m 444 dm.config ${DESTDIR}/usr/games
	install -c -o bin -g bin -m 444 dm.0 ${DESTDIR}/usr/man/cat8
	install -c -o bin -g bin -m 444 dm.config.0 ${DESTDIR}/usr/man/cat5

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

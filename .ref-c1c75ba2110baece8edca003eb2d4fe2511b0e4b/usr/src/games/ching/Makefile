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
#	@(#)Makefile	5.1 (Berkeley) %G%
#

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	cno.c phx.c
OBJS=
MAN=	ching.0

all: cno phx

cno phx: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core cno phx

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -c -o bin -g bin -m 755 ching.sh ${DESTDIR}/usr/games/ching
	-[ -d ${DESTDIR}/usr/games/ching.d ] || mkdir ${DESTDIR}/usr/games/ching.d
	install -s -o bin -g bin -m 755 cno phx ${DESTDIR}/usr/games/ching.d
	install -c -o bin -g bin -m 444 hexagrams macros ${DESTDIR}/usr/games/ching.d

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

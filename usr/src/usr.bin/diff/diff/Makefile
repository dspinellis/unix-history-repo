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
#	@(#)Makefile	4.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	diff.c diffdir.c diffreg.c
OBJS=	diff.o diffdir.o diffreg.o
HSRCS=	diffh.c
HOBJS=	diffh.o
MAN=	diff.0

all: diff diffh

diff: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

diffh: ${HOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${HOBJS}

clean:
	rm -f ${HOBJS} ${OBJS} core diff diffh

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${HSRCS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 diff ${DESTDIR}/usr/bin/diff
	install -s -o bin -g bin -m 755 diffh ${DESTDIR}/usr/bin/diffh
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/diffh.0
	ln ${DESTDIR}/usr/man/cat1/diff.0 ${DESTDIR}/usr/man/cat1/diffh.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} ${HSRCS}

tags: ${SRCS}
	ctags ${SRCS}
	ctags ${HSRCS}
	sort -o tags tags

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
#	@(#)Makefile	4.6 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	enroll.c lib.c xget.c xsend.c
OBJS=	enroll.o lib.o xget.o xsend.o
MAN=	xsend.0

all: enroll xsend xget

enroll: enroll.o lib.o ${LIBC}
	${CC} enroll.o lib.o -lmp -o $@

xget: xget.o lib.o ${LIBC}
	${CC} xget.o lib.o -lmp -o $@

xsend: xsend.o lib.o ${LIBC}
	${CC} xsend.o lib.o -lmp -o $@

clean:
	rm -f ${OBJS} core enroll xsend xget

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 enroll xget xsend \
	    ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/enroll.0
	rm -f ${DESTDIR}/usr/man/cat1/xget.0
	ln ${DESTDIR}/usr/man/cat1/xsend.0 ${DESTDIR}/usr/man/cat1/enroll.0
	ln ${DESTDIR}/usr/man/cat1/xsend.0 ${DESTDIR}/usr/man/cat1/xget.0

lint: ${SRCS}
	lint ${CFLAGS} enroll.c lib.c
	lint ${CFLAGS} xget.c lib.c
	lint ${CFLAGS} xsend.c lib.c

tags: ${SRCS
	ctags enroll.c lib.c
	ctags -a xget.c
	ctags -a xsend.c
	sort -o tags tags

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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	chpass.c field.c util.c
OBJS=	chpass.o field.o util.o
MAN=	chpass.0

all: chpass

chpass: ${LIBC} ${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean:
	rm -f ${OBJS} core chpass

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o root -g bin -m 4755 chpass ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/bin/chfn; ln ${DESTDIR}/usr/bin/chpass \
	    ${DESTDIR}/usr/bin/chfn
	rm -f ${DESTDIR}/usr/bin/chsh; ln ${DESTDIR}/usr/bin/chpass \
	    ${DESTDIR}/usr/bin/chsh
	rm -f ${DESTDIR}/usr/man/cat1/chfn.0
	rm -f ${DESTDIR}/usr/man/cat1/chsh.0
	ln ${DESTDIR}/usr/man/cat1/chpass.0 ${DESTDIR}/usr/man/cat1/chfn.0
	ln ${DESTDIR}/usr/man/cat1/chpass.0 ${DESTDIR}/usr/man/cat1/chsh.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

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
#	@(#)Makefile	5.6 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	apropos.c whatis.c man.c
MAN=	man.0 apropos.0 whatis.0

all: man apropos whatis

man: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

apropos: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

whatis: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f core apropos whatis man

cleandir: clean
	rm -f ${MAN} tags .depend

depend:
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 apropos ${DESTDIR}/usr/ucb/apropos
	install -s -o bin -g bin -m 755 whatis ${DESTDIR}/usr/ucb/whatis
	install -s -o bin -g bin -m 755 man ${DESTDIR}/usr/ucb/man
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint:
	lint ${CFLAGS} apropos.c
	lint ${CFLAGS} whatis.c
	lint ${CFLAGS} man.c

tags:
	ctags apropos.c
	ctags -a whatis.c
	ctags -a man.c
	sort -o tags tags

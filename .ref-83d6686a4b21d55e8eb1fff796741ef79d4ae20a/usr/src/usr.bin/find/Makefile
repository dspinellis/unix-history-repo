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
#	@(#)Makefile	4.7 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	find.c bigram.c code.c
OBJS=	find.o bigram.o code.o
MAN=	find.0

all: find bigram code

bigram code find: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f ${OBJS} core find bigram code

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 find ${DESTDIR}/usr/bin
	install -s -o bin -g bin -m 755 code bigram ${DESTDIR}/usr/libexec
	install -c -o bin -g bin -m 755 updatedb.csh \
	    ${DESTDIR}/usr/libexec/updatedb
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} find.c
	lint ${CFLAGS} bigram.c
	lint ${CFLAGS} code.c

tags: ${SRCS}
	ctags find.c
	ctags -a bigram.c
	ctags -a code.c
	sort -o tags tags

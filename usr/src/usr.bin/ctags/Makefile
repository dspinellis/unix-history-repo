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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
DESTDIR=
CFLAGS=-O -I.
LIBC=	/lib/libc.a
SRCS=	C.c ctags.c fortran.c lisp.c print.c tree.c yacc.c
OBJS=	C.o ctags.o fortran.o lisp.o print.o tree.o yacc.o
MAN=	ctags.0

all: ctags

ctags: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean:
	rm -f ${OBJS} core ctags

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 ctags ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

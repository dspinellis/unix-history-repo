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
#	@(#)Makefile	4.9 (Berkeley) %G%
#
#
CFLAGS=	-O -w
LIBC=	/lib/libc.a
SRCS=	setbrk.c builtin.c blok.c stak.c cmd.c fault.c main.c word.c \
	string.c name.c args.c xec.c service.c error.c io.c print.c \
	macro.c expand.c ctype.c msg.c
OBJS=	setbrk.o builtin.o blok.o stak.o cmd.o fault.o main.o word.o \
	string.o name.o args.o xec.o service.o error.o io.o print.o \
	macro.o expand.o ctype.o msg.o
MAN=	sh.0

all: sh

sh: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean:
	rm -f ${OBJS} core sh

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 sh ${DESTDIR}/bin/sh
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/case.0
	rm -f ${DESTDIR}/usr/man/cat1/for.0
	rm -f ${DESTDIR}/usr/man/cat1/if.0
	rm -f ${DESTDIR}/usr/man/cat1/while.0
	ln ${DESTDIR}/usr/man/cat1/sh.0 ${DESTDIR}/usr/man/cat1/case.0
	ln ${DESTDIR}/usr/man/cat1/sh.0 ${DESTDIR}/usr/man/cat1/for.0
	ln ${DESTDIR}/usr/man/cat1/sh.0 ${DESTDIR}/usr/man/cat1/if.0
	ln ${DESTDIR}/usr/man/cat1/sh.0 ${DESTDIR}/usr/man/cat1/while.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

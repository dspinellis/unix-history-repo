#
# Copyright (c) 1989 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	pow.c gcd.c msqrt.c mdiv.c mout.c mult.c madd.c util.c
OBJS=	pow.o gcd.o msqrt.o mdiv.o mout.o mult.o madd.o util.o
MAN=	mp.0

all: libmp.a

libmp.a: ${OBJS}
	@echo building normal libmp
	@ar cr $@ ${OBJS}
	ranlib libmp.a

clean:
	rm -f ${OBJS} core libmp.a

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -o bin -g bin -m 644 libmp.a ${DESTDIR}/usr/lib/libmp.a
	ranlib -t ${DESTDIR}/usr/lib/libmp.a
	install -c -o bin -g bin -m 444 mp.0 ${DESTDIR}/usr/man/cat3

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

FRC:

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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.14 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
HDRS=	gprof.h ${MACHINE}.h
SRCS=	gprof.c arcs.c dfn.c lookup.c ${MACHINE}.c hertz.c \
	printgprof.c printlist.c
OBJS=	gprof.o arcs.o dfn.o lookup.o ${MACHINE}.o hertz.o \
	printgprof.o printlist.o
MAN=	gprof.0

all: gprof

gprof: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

gcrt0.h: FRC
	-if [ -r gcrt0.h ] && \
		cmp -s gcrt0.h ../../lib/libc/${MACHINE}/csu/gmon.h; then \
		:; \
	else \
		rm -f gcrt0.h; \
		cp ../../lib/libc/${MACHINE}/csu/gmon.h gcrt0.h; \
	fi

clean:
	rm -f ${OBJS} core gprof

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 gprof ${DESTDIR}/usr/ucb/gprof
	install -c -o bin -g bin -m 444 gprof.flat ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 444 gprof.callg ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

FRC:

#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such redistribution and
# use acknowledge that the software was developed by the University
# of California, Berkeley.  The name of the University may not be
# used to endorse or promote products derived from this software
# without specific prior written permission.  THIS SOFTWARE IS PROVIDED
# ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.
#
# @(#)Makefile	5.1 (Berkeley) 10/22/88
#

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	ktrace.c kdump.c subr.c ioctl.c
OBJS=	ktrace.o kdump.o subr.o	ioctl.o
MAN=	ktrace.0 kdump.0

all: ktrace kdump

ktrace: ${LIBC} subr.o
	${CC} -o $@ ${CFLAGS} $@.c subr.o

kdump: ${LIBC} ioctl.o subr.o
	${CC} -o $@ ${CFLAGS} $@.c ioctl.o subr.o

ioctl.c: mkioctls
	./mkioctls > ioctl.c

clean:
	rm -f ${OBJS} core ktrace kdump

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 ktrace ${DESTDIR}/usr/bin
	install -s -o bin -g bin -m 755 kdump ${DESTDIR}/usr/bin
	: install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

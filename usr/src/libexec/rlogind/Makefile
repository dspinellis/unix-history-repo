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
# @(#)Makefile	5.1 (Berkeley) %G%
#

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	rlogind.c
OBJS=
MAN=	rlogind.0
MAN8=	/usr/man/cat8/

all: rlogind

rlogind: ${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS} -lutil

cleandir: clean
	rm -f tags .depend

clean:
	rm -f ${OBJS} core rlogind ${MAN}

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 rlogind ${DESTDIR}/etc/rlogind
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}${MAN8}${MAN}

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

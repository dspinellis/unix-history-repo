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
# @(#)Makefile	5.2 (Berkeley) 1/28/89
#

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	inetd.c
OBJS=
MAN=	inetd.0

all: inetd

inetd: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core inetd

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 inetd ${DESTDIR}/etc/inetd
	install -c -o bin -g bin -m 444 inetd.0 ${DESTDIR}/usr/man/cat8
	rm -f ${DESTDIR}/usr/man/cat5/inetd.conf.0
	ln ${DESTDIR}/usr/man/cat8/inetd.0 ${DESTDIR}/usr/man/cat5/inetd.conf.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

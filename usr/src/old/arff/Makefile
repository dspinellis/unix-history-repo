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
# @(#)Makefile	5.2 (Berkeley) %G%
#

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arff.c
OBJS=
MAN=	arff.0

all: arff flcopy

arff: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

flcopy: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core arff flcopy

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 arff ${DESTDIR}/usr/sbin
	install -s -o bin -g bin -m 755 flcopy ${DESTDIR}/usr/sbin
	install -c -o bin -g bin -m 444 arff.0 ${DESTDIR}/usr/man/cat8/vax
	rm -f ${DESTDIR}/usr/man/cat8/vax/flcopy.0
	ln ${DESTDIR}/usr/man/cat8/vax/arff.0 ${DESTDIR}/usr/man/cat8/vax/flcopy.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dlmpcc.c
MAN=	dlmpcc.0 mpcc.0 mpcctab.0
FILES=	mpcc32 mpccdl mpcctab

all: dlmpcc

dlmpcc: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f dlmpcc core a.out errs

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} dlmpcc.c

install: ${MAN}
	install -s -o bin -g bin -m 755 dlmpcc ${DESTDIR}/usr/sbin
	(cd firmware; install -c -o bin -g bin -m 444 ${FILES} ${DESTDIR}/usr/libdata)
	install -c -o bin -g bin -m 444 dlmpcc.0 ${DESTDIR}/usr/man/cat8/tahoe
	install -c -o bin -g bin -m 444 mpcc.0 ${DESTDIR}/usr/man/cat4/tahoe
	install -c -o bin -g bin -m 444 mpcctab.0 ${DESTDIR}/usr/man/cat4/tahoe

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

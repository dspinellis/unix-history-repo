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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
CSRCS=	canfield.c
CFSRCS=	cfscores.c
MAN=	canfield.0

all: canfield cfscores

canfield: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${CSRCS} -lcurses -ltermcap

cfscores: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${CFSRCS}

clean:
	rm -f core canfield cfscores

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${CSRCS} ${CFSRCS}

install: ${MAN}
	install -s -o games -g bin -m 700 canfield ${DESTDIR}/usr/games/hide
	install -s -o games -g bin -m 700 cfscores ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f canfield; ln -s dm canfield; chown games.bin canfield)
	(cd ${DESTDIR}/usr/games; rm -f cfscores; ln -s dm cfscores; chown games.bin cfscores)
	-[ -f ${DESTDIR}/usr/games/lib/cfscores ] || install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/cfscores
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6
	rm -f ${DESTDIR}/usr/man/cat6/cfscores.0
	ln ${DESTDIR}/usr/man/cat6/canfield.0 ${DESTDIR}/usr/man/cat6/cfscores.0

lint: ${SRCS}
	lint ${CFLAGS} ${CSRCS}
	lint ${CFLAGS} ${CFSRCS}

tags: ${SRCS}
	ctags ${CSRCS} ${CFSRCS}

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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
MDIR=	/usr/man/cat7
SRCS=	ascii.7 environ.7 eqnchar.7 hier.7 hostname.7 intro.7 mailaddr.7 \
	man.7 me.7 ms.7 term.7
OBJS=	ascii.0 environ.0 eqnchar.0 hier.0 hostname.0 intro.0 mailaddr.0 \
	man.0 me.0 ms.0 term.0
LINKS=	miscellaneous.0

.SUFFIXES: .7 .0

.7.0:
	${DESTDIR}/usr/man/manroff $*.7 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/intro.0 ${DESTDIR}${MDIR}/miscellaneous.0

FRC:

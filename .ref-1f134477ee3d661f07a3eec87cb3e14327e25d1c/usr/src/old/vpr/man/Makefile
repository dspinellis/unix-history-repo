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

SRCS=
OBJS=	fed.0 vfontinfo.0 vpr.0 vtroff.0 vwidth.0 vpac.0
LINKS=

.SUFFIXES: .8 .1 .0

.8.0:
	${DESTDIR}/usr/man/manroff $*.8 > $*.0
.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	install -c -o bin -g bin -m 444 ${OBJS1} ${OBJS8} ${DESTDIR}/usr/man/cat.old

FRC:


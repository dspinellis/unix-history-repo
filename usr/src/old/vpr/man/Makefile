#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
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


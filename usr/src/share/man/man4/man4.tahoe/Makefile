#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
MDIR=	/usr/man/cat4/tahoe
SRCS=	ace.4 autoconf.4 cons.4 cy.4 dr.4 enp.4 ik.4 intro.4 lp.4 mem.4 \
	mpcc.4 mpcctab.4 mtio.4 vd.4 vx.4
OBJS=	ace.0 autoconf.0 cons.0 cy.0 dr.0 enp.0 ik.0 intro.0 lp.0 mem.0 \
	mpcc.0 mpcctab.0 mtio.0 vd.0 vx.0
LINKS=	kmem.0

.SUFFIXES: .4 .0

.4.0:
	${DESTDIR}/usr/man/manroff $*.4 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/mem.0 ${DESTDIR}${MDIR}/kmem.0

FRC:

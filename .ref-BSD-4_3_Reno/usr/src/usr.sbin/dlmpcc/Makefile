#	@(#)Makefile	5.6 (Berkeley) 5/11/90

PROG=	dlmpcc
MAN8=	dlmpcc.0
MAN4=	mpcc.0 mpcctab.0
MANSUBDIR=/tahoe
FILES=	mpcc32 mpccdl mpcctab

beforeinstall:
	(cd ${.CURDIR}/firmware; install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	     ${FILES} ${DESTDIR}/usr/libdata)

.include <bsd.prog.mk>

#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	ptx

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/eign \
	    ${DESTDIR}/usr/share/dict

.include <bsd.prog.mk>

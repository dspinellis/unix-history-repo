#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	ptx

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/eign \
	    ${DESTDIR}/usr/share/dict

.include <bsd.prog.mk>

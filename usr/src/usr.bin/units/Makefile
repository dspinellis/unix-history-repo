#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	units

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/units.lib \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>

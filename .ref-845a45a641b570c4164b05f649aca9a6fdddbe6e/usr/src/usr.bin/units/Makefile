#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	units

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/units.lib \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>

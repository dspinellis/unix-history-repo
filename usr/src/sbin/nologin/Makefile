#	@(#)Makefile	5.2 (Berkeley) %G%

MAN8=	nologin.0
NOOBJ=noobj

nologin clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/nologin.sh ${DESTDIR}/sbin/nologin

.include <bsd.prog.mk>

#	@(#)Makefile	8.1 (Berkeley) %G%

NOMAN=noman
NOOBJ=noobj

all nologin clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/nologin.sh ${DESTDIR}/sbin/nologin

.include <bsd.prog.mk>

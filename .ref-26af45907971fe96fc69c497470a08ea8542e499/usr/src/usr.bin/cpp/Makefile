#	@(#)Makefile	5.1 (Berkeley) %G%

NOMAN=noman
NOOBJ=noobj

all nologin clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/cpp.sh ${DESTDIR}/usr/bin/cpp

.include <bsd.prog.mk>

#	@(#)Makefile	8.1 (Berkeley) 6/19/93

MAN8=	nologin.0

nologin clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/nologin.sh ${DESTDIR}/sbin/nologin

.include <bsd.prog.mk>
